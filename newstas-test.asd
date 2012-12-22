(asdf:defsystem newstas-test
  :version "0"
  :description "Test of newstas"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "GPL"
  :depends-on (newstas)
  :serial t
  :components ((:file "newstas-test")
               (:file "durable-test")))
