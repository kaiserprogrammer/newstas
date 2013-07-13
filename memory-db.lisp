(in-package :newstas)

(defclass memory-db ()
  ((sites :initform (list)
          :accessor sites)
   (notifications :initform (list)
                  :accessor notifications)
   (filters :initform (make-hash-table :test #'equalp)
            :accessor filters)))

(defmethod db-get-site (url (db memory-db))
  (find url (sites db) :test #'string= :key #'url))

(defmethod db-add-site ((site site) (db memory-db))
  (push site (sites db)))

(defmethod db-get-sites ((db memory-db))
  (sites db))

(defmethod db-get-notifications ((db memory-db))
  (notifications db))

(defmethod db-add-notification ((db memory-db) url)
  (push url (notifications db)))

(defmethod db-save-notifications ((db memory-db)))

(defmethod db-clear-notification (url (db memory-db))
  (setf (notifications db)
        (remove url (notifications db) :test #'string=)))

(defmethod db-clear-all-notifications ((db memory-db))
  (setf (notifications db) (list)))

(defmethod db-save-filters ((db memory-db)))

(defmethod db-update-site (site (db memory-db)))
