(asdf:defsystem newstas-multi-user
  :version "0"
  :description "Multi User Extension for Newstas"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "LLGPL"
  :depends-on (newstas dbi fiveam auser)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "newstas-multi-user")
               (:file "durable")
               (:file "durable-test"))
  ;; :long-description ""
  )
