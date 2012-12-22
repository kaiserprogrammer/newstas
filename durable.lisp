(in-package :newstas)

(defclass durable-db ()
  ((connection :accessor connection)))

(defmethod initialize-instance :after ((db durable-db) &key)
  (setf (slot-value db 'connection)
        (dbi:connect :postgres
                     :database-name (sb-posix:getenv "NEWSTASDB_NAME")
                     :username (sb-posix:getenv "NEWSTASDB_USER")
                     :password (sb-posix:getenv "NEWSTASDB_PASSWORD")
                     :host (or (sb-posix:getenv "NEWSTASDB_HOST") :unix))))

(defmethod db-save-notifications (users url (db durable-db))
  (dolist (user users)
    (dbi:execute (dbi:prepare (connection db) "insert into user_notifications values (?, ?)")
                 (id user)
                 url)))

(defmethod db-add-site ((site site) (db durable-db))
  (let ((query (dbi:prepare (connection db) "insert into sites values (?, ?)"))
        (user-site-query (dbi:prepare (connection db) "insert into user_site values (?, ?)")))
    (dbi:execute query (url site) (contents site))
    (dbi:execute user-site-query (id (car (users site))) (url site))))

(defmethod db-get-site (url (db durable-db))
  (let ((s (dbi:fetch (dbi:execute (dbi:prepare (connection db) "select * from sites where url = ?")
                                   url)))
        (users-result (dbi:execute (dbi:prepare (connection db) "select * from users where id in (select id from user_site where url = ?)")
                                   url)))
    (let ((site (make-instance 'site
                               :url (getf s :|url|))))
      (setf (contents site) (getf s :|contents|))
      (loop for row = (dbi:fetch users-result)
         while row
         do (let ((user (make-instance 'user
                                       :id (getf row :|id|))))
              (setf (slot-value user 'password) (getf row :|password|))
              (push user (users site))))
      site)))

(defmethod db-add-user ((user user) (db durable-db))
  (let ((query (dbi:prepare (connection db) "insert into users values (?, ?)")))
    (dbi:execute query (id user) (password user))))

(defmethod db-get-user (id (db durable-db))
  (let* ((u (dbi:fetch (dbi:execute (dbi:prepare (connection db) "select * from users where id = ?") id)))
         (user (make-instance 'user
                              :id (getf u :|id|))))
    (setf (slot-value user 'password) (getf u :|password|))
    (let ((sites-result (dbi:execute (dbi:prepare (connection db) "select * from sites where url in (select url from user_site where id = ?)") id)))
      (loop for row = (dbi:fetch sites-result)
         while row
         do (let ((site (make-instance 'site
                                       :url (getf row :|url|))))
              (setf (contents site) (getf row :|contents|))
              (push site (sites user)))))
    (let ((notes-result (dbi:execute (dbi:prepare (connection db) "select * from user_notifications where id = ?") id)))
      (loop for row = (dbi:fetch notes-result)
         while row
         do (push (getf row :|url|)
                  (notifications user))))
    user))

(defmethod db-clear-notifications (id (db durable-db))
  (dbi:execute (dbi:prepare (connection db) "delete from user_notifications where id = ?")
               id))

(defmethod db-clear-notification (id url (db durable-db))
  (dbi:execute (dbi:prepare (connection db) "delete from user_notifications where id = ? and url = ?")
               id url))

(defmethod drop-tables ((db durable-db))
  (ignore-errors (dbi:do-sql (connection db) "drop table users"))
  (ignore-errors (dbi:do-sql (connection db) "drop table sites"))
  (ignore-errors (dbi:do-sql (connection db) "drop table user_site"))
  (ignore-errors (dbi:do-sql (connection db) "drop table user_notifications")))

(defmethod create-tables ((db durable-db))
  (dbi:do-sql (connection db) "create table users (id varchar(50), password varchar(255), primary key (id))")
  (dbi:do-sql (connection db) "create table sites (url varchar(255), contents text, primary key (url))")
  (dbi:do-sql (connection db) "create table user_site (id varchar(50), url varchar(255))")
  (dbi:do-sql (connection db) "create table user_notifications (id varchar(50), url varchar(255))"))

(defmethod recreate-tables ((db durable-db))
  (drop-tables db)
  (create-tables db))

(defmacro with-durable-db ((var) &body body)
  `(let ((,var (make-instance 'durable-db)))
     (unwind-protect
          (progn ,@body)
       (dbi:disconnect (connection ,var)))))
