(in-package :newstas)

(defgeneric db-get-site (url db))

(defgeneric db-get-sites (db))

(defgeneric db-add-site (site db))

(defgeneric db-save-notifications (db))

(defgeneric db-add-notification (url db))

(defgeneric db-get-notifications (db))

(defgeneric db-clear-notification (url db))

(defgeneric db-clear-notifications (db))

(defgeneric db-save-filters (db))

(defgeneric db-update-site (site db))
