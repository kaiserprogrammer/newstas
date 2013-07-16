(in-package :newstas-multi-user)

(def-suite durable)
(in-suite durable)

;; (test persisting-user
;;   (with-durable-db (*db*)
;;     (recreate-tables *db*))
;;   (with-durable-db (*db*)
;;     (is (not (null (db-get-user "blub" *db*))))
;;     (is (verify-user "blub" "secret") "password verification failed")
;;     (is (not (verify-user "blub" "wrong_pw")))))

(test persisteng-site
  (let ((*user-id* 1))
   (with-durable-db (*db*)
     (recreate-tables *db*)
     (let ((*data-retriever*
            (lambda (url) (declare (ignore url))
               "content")))
       (add-site "http://example.com"))
     (let ((*data-retriever*
            (lambda (url) (declare (ignore url))
               "new-content")))
       (check-site "http://example.com")))
    (with-durable-db (*db*)
      (let ((site (newstas::db-get-site *db* "http://example.com")))
        (is (not (null site)))
        (is (string= "http://example.com" (newstas::url site)))
        (is (string= "new-content" (newstas::contents site)))))))

(test persisting-notifications
  (let ((*user-id* 1))
   (with-durable-db (*db*)
     (recreate-tables *db*)
     (let ((*data-retriever*
            (lambda (url) url)))
       (add-site "http://example.com")))
   (with-durable-db (*db*)
     (let ((*data-retriever*
            (lambda (url) (declare (ignore url)) "content")))
       (check-site "http://example.com")))
   (with-durable-db (*db*)
     (let ((notes (get-notifications)))
       (is (not (null notes)))
       (is (equalp (list "http://example.com")
                   notes))
       (clear-all-notifications)))
   (with-durable-db (*db*)
     (is (null (get-notifications)))
     (let ((*data-retriever*
            (lambda (url) (declare (ignore url)) "new-content")))
       (check-site "http://example.com")
       (add-site "http://blub.com"))
     (let ((*data-retriever*
            (lambda (url) url)))
       (check-site "http://blub.com")))
   (with-durable-db (*db*)
     (is (not (null (get-notifications))))
     (clear-notification "http://example.com"))
   (with-durable-db (*db*)
     (is (equal (list "http://blub.com")
                (get-notifications))))))

(test persisting-filters
  (let ((*user-id* 1))
    (with-durable-db (*db*)
      (recreate-tables *db*)
      (let ((*data-retriever*
             (lambda (url) (declare (ignore url))
                "random /from_here_to_there.blub")))
        (add-site "http://example.com"))
      (newstas::add-content-filter-include "http://example.com"
                                          :from "/"
                                          :to "\\."))
    (with-durable-db (*db*)
      (let ((*data-retriever*
             (lambda (url) (declare (ignore url))
                "different_but_uninteresting/from_here_to_there.different")))
        (check-site "http://example.com"))
      (is (null (get-notifications))))))

(test news-for-multiple-users
  (with-durable-db (*db*)
    (recreate-tables *db*)
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url))
              "content")))
      (let ((*user-id* 1))
        (add-site "http://example.com"))
      (let ((*user-id* 2))
        (add-site "http://different.com")))
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url))
              "new-content")))
      (news)
      (let ((*user-id* 1))
        (is (not (null (get-notifications)))))
      (let ((*user-id* 2))
        (is (not (null (get-notifications))))))
    (let ((*user-id* 1))
      (clear-all-notifications))
    (let ((*user-id* 2))
      (clear-all-notifications))
    (let ((*data-retriever*
           (lambda (url)
             (if (string= url "http://different.com")
                 "newer-content"
                 "new-content"))))
      (news)
      (let ((*user-id* 1))
        (is (null (get-notifications))))
      (let ((*user-id* 2))
        (is (not (null (get-notifications))))))))
(run!)
