(defpackage #:newstas
  (:use #:cl :fiveam))

(in-package :newstas)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)
   (sites :initform (make-hash-table :test #'equal)
          :accessor sites)))

(defclass site ()
  ((url :initarg :url
        :reader url)
   (contents :accessor contents)
   (users :initform (list)
          :accessor users)))

(defclass user ()
  ((id :initarg :id
       :reader id)
   (password :initarg :password
             :reader password)
   (sites :initform (list)
          :accessor sites)
   (notifications :initform (list)
                  :accessor notifications)))

(defvar *data-retriever*
  (lambda (url)
    (drakma:http-request url)))

(defvar *hasher*
  (lambda (password)
    (ironclad:pbkdf2-hash-password-to-combined-string
     (ironclad:ascii-string-to-byte-array password))))

(defvar *checker*
  (lambda (hash password)
    (ironclad:pbkdf2-check-password
     (ironclad:ascii-string-to-byte-array password)
     hash)))

(defmethod (setf password) (pw (user user))
  (setf (slot-value user 'password) (funcall *hasher* pw)))

(defmethod initialize-instance :after ((u user) &key)
  (setf (password u) (password u)))

(defun db-get-user (id db)
  (gethash id (users db)))

(defun db-add-user (user db)
  (setf (gethash (id user) (users db)) user))

(defun db-get-site (url db)
  (gethash url (sites db)))

(defun db-add-site (site db)
  (setf (gethash (url site) (sites db)) site))

(defvar *db*)

(defun add-user (id password &optional (db *db*))
  (let ((user (make-instance 'user
                             :id id
                             :password password)))
    (db-add-user user db)))

(defun verify-user (id password &optional (db *db*))
  (funcall *checker* (password (db-get-user id db)) password))

(defun add-site (id url &optional (db *db*))
  (let ((contents (funcall *data-retriever* url)))
    (when contents
      (let ((site (make-instance 'site
                                 :url url))
            (user (db-get-user id db)))
        (push site (sites user))
        (push user (users site))
        (db-add-site site db)
        (setf (contents site) contents)))))

(defun notify-users (site)
  (loop for user in (users site)
     do (notify-user user (url site))))

(defun notify-user (user url)
  (push url (notifications user)))

(defun news-for (id &optional (db *db*))
  (mapcar
   (lambda (site) (check-site (url site) db))
   (sites (db-get-user id db))))

(defun check-site (url &optional (db *db*))
  (let* ((site (db-get-site url db))
         (new-contents (funcall *data-retriever* (url site))))
    (unless (string= new-contents (contents site))
      (setf (contents site) new-contents)
      (notify-users site))))

(defun get-notifications (id &optional (db *db*))
  (let ((user (db-get-user id db)))
    (notifications user)))

