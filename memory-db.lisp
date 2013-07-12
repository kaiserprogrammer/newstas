(in-package :newstas)

(defmethod db-get-site (url (db memory-db))
  (find url (sites db) :test #'string= :key #'url))

(defmethod db-add-site ((site site) (db memory-db))
  (push site (sites db)))

(defmethod db-save-notifications ((db memory-db)))

(defmethod db-clear-notification (url (db memory-db)))

(defmethod db-clear-notifications ((db memory-db)))

(defmethod db-save-filters ((db memory-db)))

(defmethod db-update-site (site (db memory-db)))
