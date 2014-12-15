(require 'asdf)

(asdf:defsystem amarok-control
  :depends-on (:hunchentoot
               :cl-who
               :split-sequence
               :cl-ppcre)
  :components ( (:file "main" :depends-on ("controller"))
                (:file "controller")))