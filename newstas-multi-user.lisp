(defpackage #:newstas-multi-user
  (:use #:cl #:newstas #:fiveam)
  (:shadow :news)
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
   :clear-notification
   :durable-db
   :with-durable-db
   :*user-id*))

(in-package :newstas-multi-user)

(defvar *user-id*)

(defun news (&optional (db *db*))
  (dolist (site (newstas::db-get-sites db))
    (let* ((url (newstas::url site))
           (new-contents (funcall *data-retriever* url)))
      (unless (string= (funcall *content-filter* new-contents)
                       (funcall *content-filter* (newstas::contents site)))
        (notify-users site new-contents db)
        (newstas::db-update-site db url new-contents)))))

(defun notify-users (site new-contents db)
  (dolist (id (db-users-subscribed db site))
    (let ((*user-id* id))
      (newstas::db-add-notification db (newstas::url site)))))
