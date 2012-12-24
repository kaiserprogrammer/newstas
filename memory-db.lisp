(in-package :newstas)

(defclass memory-db ()
  ((users :initform (make-hash-table :test #'equal)
          :accessor users)
   (sites :initform (make-hash-table :test #'equal)
          :accessor sites)))

(defmethod db-get-user (id (db memory-db))
  (gethash id (users db)))

(defmethod db-add-user ((user user) (db memory-db))
  (setf (gethash (id user) (users db)) user))

(defmethod db-get-site (url (db memory-db))
  (gethash url (sites db)))

(defmethod db-add-site ((site site) (db memory-db))
  (setf (gethash (url site) (sites db)) site))

(defmethod db-save-notifications (user (db memory-db)))

(defmethod db-clear-notification (id url (db memory-db)))

(defmethod db-clear-notifications (id (db memory-db)))

(defmethod db-save-filters (id (db memory-db)))
