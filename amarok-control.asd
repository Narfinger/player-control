(require 'asdf)

(asdf:defsystem amarok-control
  :depends-on (:hunchentoot
               :cl-who)
  :components ( (:file "main" :depends-on ("controller"))
                (:file "controller")))