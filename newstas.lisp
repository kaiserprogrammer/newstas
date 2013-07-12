(defpackage #:newstas
  (:use #:cl #:fiveam)
  (:export
   :memory-db
   :*db*
   :*data-retriever*
   :*hasher*
   :*checker*
   :add-site
   :news
   :check-site
   :get-notifications
   :*content-filter*
   :clear-notifications
   :clear-notification
   :durable-db
   :with-durable-db))

(in-package :newstas)

(defclass site ()
  ((url :initarg :url
        :reader url)
   (contents :accessor contents)
   (users :initform (list)
          :accessor users)))

(defclass memory-db ()
  ((sites :initform (list)
          :accessor sites)
   (notifications :initform (list)
                  :accessor notifications)
   (filters :initform (make-hash-table :test #'equalp)
            :accessor filters)))

(defclass content-filter-include ()
  ((from :initarg :from
         :reader from)
   (to :initarg :to
       :reader to)))

(defvar *db*)

(defparameter *content-filter*
  (lambda (content)
    (let ((content-length (length content)))
      (subseq content
              (or (cl-ppcre:scan "<body" content)
                  0)
              (or (cl-ppcre:scan "(</body|</html>)" content :end content-length)
                  content-length)))))

(defparameter *data-retriever*
  (lambda (url)
    (drakma:http-request url)))

(defun add-site (url &optional (db *db*))
  (let ((contents (funcall *data-retriever* url)))
    (when contents
      (let ((site (make-instance 'site
                                 :url url)))
        (push site (sites db))
        (setf (contents site) contents)
        (db-add-site site db)))))

(defun notify (url old new &optional (db *db*))
  (let ((filter (gethash url (filters db))))
    (let ((old (apply-filter filter old))
          (new (apply-filter filter new)))
      (unless (string= old new)
        (push url (notifications db))))))

(defun news (&optional (db *db*))
  (mapcar
   (lambda (site) (check-site (url site) db))
   (sites db)))


(defun check-site (url &optional (db *db*))
  (let* ((site (db-get-site url db))
         (new-contents (funcall *data-retriever* (url site))))
    (unless (string= (funcall *content-filter* new-contents)
                     (funcall *content-filter* (contents site)))
      (let ((old (contents site)))
        (setf (contents site) new-contents)
        (notify (url site) old (contents site) db)
        (db-update-site site db)
        (db-save-notifications db)))))

(defun get-notifications (&optional (db *db*))
  (notifications db))

(defun clear-notifications (&optional (db *db*))
  (setf (notifications db) (list))
  (db-clear-notifications db))

(defun clear-notification (url &optional (db *db*))
  (setf (notifications db)
        (remove url (notifications db) :test #'string=))
  (db-clear-notification url db))

(defun add-content-filter-include (url &key (db *db*) from to)
  (setf (gethash url (filters db))
        (make-instance 'content-filter-include
                       :from from
                       :to to))
  (db-save-filters db))

(defgeneric apply-filter (filter content))
(defmethod apply-filter ((f (eql nil)) content)
  content)

(defmethod apply-filter ((f content-filter-include) content)
  (let* ((start (cl-ppcre:scan (from f) content))
         (end (cl-ppcre:scan (to f) content :start start)))
    (subseq content
            start
            end)))
