(asdf:defsystem newstas
  :version "0"
  :description "Notifys the user when the internet changes"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "GPL"
  :depends-on (drakma ironclad fiveam)
  :serial t
  ;; components likely need manual reordering
  :components ((:static-file "README.md" :pathname "README.md")
               (:file "newstas")))
