(in-package :newstas)

(defvar *user-id*)

(defclass durable-db ()
  ((connection :accessor connection)))

(defmethod initialize-instance :after ((db durable-db) &key)
  (setf (slot-value db 'connection)
        (dbi:connect :postgres
                     :database-name (sb-posix:getenv "NEWSTASDB_NAME")
                     :username (sb-posix:getenv "NEWSTASDB_USER")
                     :password (sb-posix:getenv "NEWSTASDB_PASSWORD")
                     :host (or (sb-posix:getenv "NEWSTASDB_HOST") :unix))))

(defmethod db-add-site ((db durable-db) (site site))
  (let ((query (dbi:prepare (connection db) "insert into sites values (?, ?)"))
        (user-site-query (dbi:prepare (connection db) "insert into user_site values (?, ?)")))
    (dbi:execute query (url site) (contents site))
    (dbi:execute user-site-query *user-id* (url site))))

(defmethod db-get-site ((db durable-db) url)
  (let ((s (dbi:fetch (dbi:execute (dbi:prepare (connection db) "select * from sites where url = ?")
                                   url))))
    (let ((site (make-instance 'site
                               :url (getf s :|url|))))
      (setf (contents site) (getf s :|contents|))
      site)))

(defmethod db-update-site ((db durable-db) url new-content)
  (dbi:execute (dbi:prepare (connection db) "update sites set contents=? where url = ?")
               new-content
               url))

(defmethod db-add-notification ((db durable-db) url)
  (dbi:execute (dbi:prepare (connection db) "insert into user_notifications values (?, ?)")
               *user-id*
               url))

(defmethod db-get-notifications ((db durable-db))
  (let ((s (dbi.driver:fetch-all (dbi:execute (dbi:prepare (connection db) "select url from user_notifications where id = ?")
                                   *user-id*))))

    (mapcar (lambda (s) (getf s :|url|)) s)))

(defmethod db-clear-all-notifications ((db durable-db))
  (dbi:execute (dbi:prepare (connection db) "delete from user_notifications where id = ?")
               *user-id*))

(defmethod db-clear-notification ((db durable-db) url)
  (dbi:execute (dbi:prepare (connection db) "delete from user_notifications where id = ? and url = ?")
               *user-id* url))

(defmethod db-add-filter ((db durable-db) filter)
  (dbi:execute (dbi:prepare (connection db) "insert into filters values (?, ?, ?, ?, ?)")
               *user-id*
               (url filter)
               (filter-type filter db)
               (from filter)
               (to filter)))

(defmethod db-get-filter ((db durable-db) url)
  (let ((s (dbi:fetch (dbi:execute (dbi:prepare (connection db) "select * from filters where id = ? and url = ?")
                                   *user-id*
                                   url))))
    (when s
      (make-instance (intern (string-upcase (getf s :|filter_type|)))
                     :from (getf s :|filter_from|)
                     :to (getf s :|filter_to|)
                     :url (getf s :|url|)))))

(defmethod filter-type ((f content-filter-include) (db durable-db))
  (format nil "~s" (class-name (class-of f))))

(defmethod drop-tables ((db durable-db))
  (ignore-errors (dbi:do-sql (connection db) "drop table users"))
  (ignore-errors (dbi:do-sql (connection db) "drop table sites"))
  (ignore-errors (dbi:do-sql (connection db) "drop table user_site"))
  (ignore-errors (dbi:do-sql (connection db) "drop table user_notifications"))
  (ignore-errors (dbi:do-sql (connection db) "drop table filters")))

(defmethod create-tables ((db durable-db))
  (dbi:do-sql (connection db) "create table users (id varchar(50), password varchar(255), primary key (id))")
  (dbi:do-sql (connection db) "create table sites (url varchar(255), contents text, primary key (url))")
  (dbi:do-sql (connection db) "create table user_site (id varchar(50), url varchar(255))")
  (dbi:do-sql (connection db) "create table user_notifications (id varchar(50), url varchar(255))")
  (dbi:do-sql (connection db) "create table filters (id varchar(50), url varchar(255), filter_type varchar(255), filter_from varchar(255), filter_to varchar(255))"))

(defmethod recreate-tables ((db durable-db))
  (drop-tables db)
  (create-tables db))

(defmacro with-durable-db ((var) &body body)
  `(let ((,var (make-instance 'durable-db)))
     (unwind-protect
          (progn ,@body)
       (dbi:disconnect (connection ,var)))))

