(in-package :newstas)

(defclass memory-db ()
  ((sites :initform (make-hash-table :test #'equalp)
          :accessor sites)
   (notifications :initform (list)
                  :accessor notifications)
   (filters :initform (make-hash-table :test #'equalp)
            :accessor filters)))

(defmethod db-get-site ((db memory-db) url)
  (gethash url (sites db)))

(defmethod db-add-site ((db memory-db) (site site))
  (setf (gethash (url site) (sites db)) site))

(defmethod db-get-sites ((db memory-db))
  (loop for k being the hash-key using (hash-value v) in (sites db)
     collect v))

(defmethod db-get-notifications ((db memory-db))
  (notifications db))

(defmethod db-add-notification ((db memory-db) url)
  (push url (notifications db)))

(defmethod db-save-notifications ((db memory-db)))

(defmethod db-clear-notification ((db memory-db) url)
  (setf (notifications db)
        (remove url (notifications db) :test #'string=)))

(defmethod db-clear-all-notifications ((db memory-db))
  (setf (notifications db) (list)))

(defmethod db-add-filter ((db memory-db) filter)
  (setf (gethash (url filter) (filters db)) filter))

(defmethod db-update-site ((db memory-db) url new-content)
  (setf (contents (gethash url (sites db))) new-content))

(defmethod db-get-filter ((db memory-db) url)
  (gethash url (filters db)))
