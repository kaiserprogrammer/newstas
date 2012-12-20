(in-package :newstas)

(def-suite newstas)
(in-suite newstas)

(test adding-a-user
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((user (db-get-user "blub" *db*)))
      (is (not (null user)))
      (is (string= "blub" (id user)))
      (is (not (string= "secret" (password user)))))))

(test verifying-password
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (is-true (verify-user "blub" "secret"))))

(test adding-a-site
  (let ((*db* (make-instance 'memory-db))
        (url "http://www.example.com"))
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "Fine")))
      (add-site "blub" url))
    (let* ((user (db-get-user "blub" *db*))
           (site (car (sites user))))
      (is (not (null site)))
      (is (eql (db-get-site url *db*)
               site))
      (is (string= url
                   (url site)))
      (is (string= "Fine"
                   (contents site))))))

(test not-adding-a-site
  "when valid data cannot be retrieved"
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             nil)))
      (add-site "blub" "not_valid_url"))
    (is (null (car (sites (db-get-user "blub" *db*)))))
    (is (null (db-get-site "not_valid_url" *db*)))))

(test news-for
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "No Change")))
      (add-site "blub" "valid_url")
      (news-for "blub"))
    (is (null (notifications (db-get-user "blub" *db*))))

    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "Change")))
      (news-for "blub"))
    (is (not (null (notifications (db-get-user "blub" *db*)))))))

(test check-site
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "No Change")))
      (add-site "blub" "valid_url")
      (check-site "valid_url"))
    (is (null (notifications (db-get-user "blub" *db*))))

    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "Change")))
      (check-site "valid_url"))
    (is (not (null (notifications (db-get-user "blub" *db*)))))))

(test getting-notifications
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (is (null (get-notifications "blub")))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "content")))
      (add-site "blub" "url"))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "new content")))
      (check-site "url"))
    (let ((note (car (get-notifications "blub"))))
      (is (not (null note)))
      (is (string= "url" note)))))

(test content-filter
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "<html><meta>changingAB</meta><body>same_content")))
      (add-site "blub" "url"))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "<html><meta>changingBA</meta><body>same_content")))
      (check-site "url"))
    (is (null (get-notifications "blub")))))

(test clearing-notifications
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "not changed")))
      (add-site "blub" "url"))
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "changed")))
      (check-site "url"))
    (clear-notifications "blub")
    (is (null (get-notifications "blub")))))

(test clearing-one-notification
  (let ((*db* (make-instance 'memory-db)))
    (add-user "blub" "secret")
    (let ((*data-retriever*
           (lambda (url)
             (declare (ignore url))
             "not changed")))
      (add-site "blub" "url")
      (add-site "blub" "url2"))
    (let ((*data-retriever*
           (lambda (url) url)))
      (news-for "blub"))
    (clear-notification "blub" "url")
    (is (equal (list "url2") (get-notifications "blub")))))

(run!)
