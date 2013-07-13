(in-package :newstas)

(defgeneric db-get-site (db url))

(defgeneric db-get-sites (db))

(defgeneric db-add-site (db site))

(defgeneric db-update-site (db url new-content))

(defgeneric db-save-notifications (db))

(defgeneric db-add-notification (db url))

(defgeneric db-get-notifications (db))

(defgeneric db-clear-notification (db url))

(defgeneric db-clear-all-notifications (db))

(defgeneric db-add-filter (db filter))

(defgeneric db-get-filter (db url))
