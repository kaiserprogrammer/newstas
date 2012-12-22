(in-package :newstas)

(defgeneric db-get-user (id db))

(defgeneric db-add-user (user db))

(defgeneric db-get-site (url db))

(defgeneric db-add-site (site db))

(defgeneric db-save-notifications (users url db))

(defgeneric db-clear-notification (id url db))

(defgeneric db-clear-notifications (id db))