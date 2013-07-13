(in-package :newstas)

(def-suite newstas)
(in-suite newstas)

(test adding-a-site
  (let ((*db* (make-instance 'memory-db))
        (url "http://www.example.com"))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "Fine")))
      (add-site url))
    (let ((site (gethash url (sites *db*))))
      (is (not (null site)))
      (is (eql (db-get-site *db* url)
               site))
      (is (string= url
                   (url site)))
      (is (string= "Fine"
                   (contents site))))))

(test not-adding-a-site
  "when valid data cannot be retrieved"
  (let ((*db* (make-instance 'memory-db)))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             nil)))
      (add-site "not_valid_url"))
    (is (null (db-get-site *db* "not_valid_url")))))

(test news-for
  (let ((*db* (make-instance 'memory-db)))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "No Change")))
      (add-site "valid_url")
      (news))
    (is (null (notifications *db*)))

    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "Change")))
      (news))
    (is (not (null (notifications *db*))))))

(test check-site
  (let ((*db* (make-instance 'memory-db)))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "No Change")))
      (add-site "valid_url")
      (check-site "valid_url"))
    (is (null (notifications *db*)))

    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "Change")))
      (check-site "valid_url"))
    (is (not (null (notifications *db*))))))

(test getting-notifications
  (let ((*db* (make-instance 'memory-db)))
    (is (null (get-notifications)))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "content")))
      (add-site "url"))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "new content")))
      (check-site "url"))
    (let ((note (car (get-notifications))))
      (is (not (null note)))
      (is (string= "url" note)))))

(test content-filter
  (let ((*db* (make-instance 'memory-db)))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "<html><meta>changingAB</meta><body id=\"blub\">same_content")))
      (add-site "url"))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "<html><meta>changingBA</meta><body id=\"blub\">same_content")))
      (check-site "url"))
    (is (null (get-notifications)))))

(test clearing-notifications
  (let ((*db* (make-instance 'memory-db)))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "not changed")))
      (add-site "url"))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "changed")))
      (check-site "url"))
    (clear-all-notifications)
    (is (null (get-notifications)))))

(test clearing-one-notification
  (let ((*db* (make-instance 'memory-db)))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "not changed")))
      (add-site "url")
      (add-site "url2"))
    (let ((*data-retriever*
           (lambda (url) url)))
      (news))
    (clear-notification "url")
    (is (equal (list "url2") (get-notifications)))))

(test configure-site-for-user-with-different-content-filter
  (let ((*db* (make-instance 'memory-db)))
    (let ((*data-retriever*
           (lambda (url) url)))
      (add-site "http://example.com")
      (add-content-filter-include "http://example.com" :from "exam" :to "\\.")
      (check-site "http://example.com"))
    (is (null (notifications *db*)) "content-filter acted not on previous content")
    (let ((*data-retriever*
           (lambda (url) (declare (ignore url))
              "different://example.different")))
      (check-site "http://example.com"))
    (is (null (notifications *db*)) "content-filter included wrong content")))

(run!)
