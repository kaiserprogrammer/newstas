(defpackage #:newstas
  (:use #:cl #:fiveam)
  (:export
   :memory-db
   :*db*
   :*data-retriever*
   :add-site
   :news
   :check-site
   :get-notifications
   :*content-filter*
   :clear-all-notifications
   :clear-notification))

(in-package :newstas)

(defclass site ()
  ((url :initarg :url
        :reader url)
   (contents :accessor contents)
   (users :initform (list)
          :accessor users)))

(defclass content-filter-include ()
  ((from :initarg :from
         :reader from)
   (to :initarg :to
       :reader to)
   (url :initarg :url
        :reader url)))

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
        (setf (contents site) contents)
        (db-add-site db site)))))

(defun notify (url old new db)
  (let ((filter (db-get-filter db url)))
    (let ((old (apply-filter filter old))
          (new (apply-filter filter new)))
      (unless (string= old new)
        (db-add-notification db url)))))

(defun news (&optional (db *db*))
  (mapcar
   (lambda (site) (check-site (url site) db))
   (db-get-sites db)))

(defun check-site (url &optional (db *db*))
  (let* ((site (db-get-site db url))
         (new-contents (funcall *data-retriever* url)))
    (unless (string= (funcall *content-filter* new-contents)
                     (funcall *content-filter* (contents site)))
      (let ((old (contents site)))
        (notify (url site) old new-contents db)
        (db-update-site db url new-contents)))))

(defun get-notifications (&optional (db *db*))
  (db-get-notifications db))

(defun clear-all-notifications (&optional (db *db*))
  (db-clear-all-notifications db))

(defun clear-notification (url &optional (db *db*))
  (db-clear-notification db url))

(defun add-content-filter-include (url &key (db *db*) from to)
  (db-add-filter
   db
   (make-instance 'content-filter-include
                  :from from
                  :to to
                  :url url)))

(defgeneric apply-filter (filter content))
(defmethod apply-filter ((f (eql nil)) content)
  content)

(defmethod apply-filter ((f content-filter-include) content)
  (let* ((start (cl-ppcre:scan (from f) content))
         (end (cl-ppcre:scan (to f) content :start start)))
    (subseq content
            start
            end)))
