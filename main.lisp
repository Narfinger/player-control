(in-package :cl-user)
(require :hunchentoot)
(require :cl-who)

(in-package :amarok-control)


(setf *dispatch-table*
      (list #'dispatch-easy-handlers
            #'default-dispatcher))
;; (setq hunchentoot:*dispatch-table*
;;       (list (hunchentoot:create-prefix-dispatcher "/amarok.html" 'index-dispatcher)
;;             (hunchentoot:create-prefix-dispatcher "/test.html" 'testpage)))
;(defvar *ht-server* (hunchentoot:start-server :port 8080))

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username "nobody") (string= password "password"))
            ,@body)
           (t (hunchentoot:require-authorization "amarok-control")))))

;; (defun generate-index-page ()
;;   (with-http-authentication
;;       (with-html-output (*standard-output* nil :indent t)
;;         (:html
;;          (:head
;;           (:title "Amarok Control"))
;;          ;; color is not right i think
;;          (:body :bgcolor "white" :color "green"
;;                 (:p "Dies ist ein Test"))))))

;; (defun index-dispatcher ()
;;   (cond 
;;     ((eq (hunchentoot:request-method) :POST)
;;      (testpage))
;;     ((eq (hunchentoot:request-method) :GET)
;;      (generate-index-page nil))))

;(defun generate-index-page ( additional-message )
(define-easy-handler (amarok :uri "/amarok" :default-request-type :POST)
    ((what :parameter-type 'string))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:html
     (:head
      (:title "Amarok Control"))
     ;; color is not right i think
     (:body :bgcolor "white" :color "black"
            (:h1 "Amarok Control")
            (:table :border 3
                    (:tr
                     (:td 
                      (:form :action "amarok" :method "post"
                             (:input :type "submit" :value "Previous")
                             (:input :type "hidden" :name "what" :value "prev")))
                    (:td 
                     (:form :action "amarok" :method "post"
                            (:input :type "submit" :value "Next")
                            (:input :type "hidden" :name "what" :value "next")))
                    (:td
                     (:form :action "amarok" :method "post"
                            (:input :type "submit" :value "Play/Pause")
                            (:input :type "hidden" :name "what" :value "pp")))
                    (:td
                     (:form :action "amarok" :method "post"
                            (:input :type "submit" :value "Stop")
                            (:input :type "hidden" :name "what" :value "stop")))))
                    (unless (null what)
                      (htm (:p (str what) )))))))
         

;different forms with different fields could also work
(defun testpage ()
  (let ((action (hunchentoot:parameter "what")))
    (when (null action)
      (setq action "NO ACTION"))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:html (:head (:title "test")) 
           (:body :bgcolor "white" (:h1 "TOEST")
                  (:p (fmt  "blubber ~A" action)))))))

;; can use (redirect "amarok.html"
(defun amarok-previous ()
  (with-html-output (*standard-output* nil :indent t)
    (:html
     (:head
      (:title "Amarok Control"))
     ;; color is not right i think
     (:body :bgcolor "white" :color "green"
            (:h1 "Amarok Control")))))
    
  
