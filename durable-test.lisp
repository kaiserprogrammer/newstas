(in-package :newstas)

(def-suite durable :in newstas)
(in-suite durable)

(test persisting-user
  (with-durable-db (*db*)
    (recreate-tables *db*)
    (add-user "blub" "secret"))
  (with-durable-db (*db*)
    (is (not (null (db-get-user "blub" *db*))))
    (is (verify-user "blub" "secret") "password verification failed")
    (is (not (verify-user "blub" "wrong_pw")))))

(test persisteng-site
  (with-durable-db (*db*)
    (recreate-tables *db*)
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url))
              "content")))
      (add-site "blub" "http://example.com")))
  (with-durable-db (*db*)
    (let ((site (db-get-site "http://example.com" *db*)))
      (is (not (null site)))
      (is (string= "http://example.com" (url site)))
      (is (string= "content" (contents site)))
      (is (not (null (users site)))))))

(test persisting-notifications
  (with-durable-db (*db*)
    (recreate-tables *db*)
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url) url)))
      (add-site "blub" "http://example.com")))
  (with-durable-db (*db*)
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url)) "content")))
      (check-site "http://example.com")))
  (with-durable-db (*db*)
    (let ((notes (notifications (db-get-user "blub" *db*))))
      (is (not (null notes)))
      (is (equalp (list "http://example.com")
                  notes))
      (clear-notifications "blub")))
  (with-durable-db (*db*)
    (is (null (notifications (db-get-user "blub" *db*))))
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url)) "new-content")))
      (check-site "http://example.com")
      (add-site "blub" "http://blub.com"))
    (let ((*data-retriever*
           (lambda (url) url)))
      (check-site "http://blub.com")))
  (with-durable-db (*db*)
    (is (not (null (notifications (db-get-user "blub" *db*)))))
    (clear-notification "blub" "http://example.com"))
  (with-durable-db (*db*)
    (is (equal (list "http://blub.com")
               (notifications (db-get-user "blub" *db*))))))

(test persisting-filters
  (with-durable-db (*db*)
    (recreate-tables *db*)
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url))
              "random /from_here_to_there.blub")))
      (add-site "blub" "http://example.com"))
    (add-content-filter-include "blub" "http://example.com"
                                :from "/"
                                :to "\\."))
  (with-durable-db (*db*)
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url))
              "different_but_uninteresting/from_here_to_there.different")))
      (check-site "http://example.com"))
    (is (null (notifications (db-get-user "blub" *db*))))))

(run!)
