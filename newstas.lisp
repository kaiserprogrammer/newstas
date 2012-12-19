(defpackage #:newstas
  (:use #:cl :fiveam))

(in-package :newstas)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)))

(defclass user ()
  ((id :initarg :id
       :accessor id)
   (password :initarg :password
             :reader password)))

(defun hash-password (password)
  (ironclad:pbkdf2-hash-password-to-combined-string
   (ironclad:ascii-string-to-byte-array password)))

(defmethod (setf password) (pw (user user))
  (setf (slot-value user 'password) (hash-password pw)))

(defmethod initialize-instance :after ((u user) &key)
  (setf (password u) (password u)))

(defun db-get-user (id &optional (db *db*))
  (gethash id (users db)))

(defun db-add-user (user &optional (db *db*))
  (setf (gethash (id user) (users db)) user))

(defvar *db*)

(defun add-user (id password &optional (db *db*))
  (let ((user (make-instance 'user
                             :id id
                             :password password)))
    (db-add-user user db)))

(defun verify-user (id password &optional (db *db*))
  (check-password (password (db-get-user id db)) password))

(defun check-password (hash password)
  (ironclad:pbkdf2-check-password (ironclad:ascii-string-to-byte-array password) hash))

(def-suite newstas)
(in-suite newstas)

(test adding-a-user
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((user (db-get-user "blub")))
      (is (not (null user)))
      (is (string= "blub" (id user)))
      (is (not (string= "secret" (password user)))))))

(test verifying-password
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (is-true (verify-user "blub" "secret"))))

(run!)
