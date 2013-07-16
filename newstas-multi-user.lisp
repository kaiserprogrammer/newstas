(defpackage #:newstas-multi-user
  (:use #:cl #:newstas #:fiveam)
  (:shadow :news))

(in-package :newstas-multi-user)

(defun news (&optional (db *db*))
  (dolist (site (newstas::db-get-sites db))
    (let* ((url (newstas::url site))
           (new-contents (funcall *data-retriever* url)))
      (unless (string= (funcall *content-filter* new-contents)
                       (funcall *content-filter* (newstas::contents site)))
        (notify-users site new-contents db)
        (newstas::db-update-site db url new-contents)))))

(defun notify-users (site new-contents db)
  (dolist (*user-id* (db-users-subscribed db site))
    (newstas::db-add-notification db (newstas::url site))))
