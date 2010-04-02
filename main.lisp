(in-package :cl-user)
(require :hunchentoot)
(require :cl-who)

(in-package :amarok-control)


(setf *dispatch-table*
      (list #'dispatch-easy-handlers
            #'default-dispatcher))

(push (create-static-file-dispatcher-and-handler "/style.css" "./style.css")
      *dispatch-table*)


;; (push (hunchentoot:create-prefix-dispatcher "/execute" 'execute-page)
;;       hunchentoot:*dispatch-table*)
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
(define-easy-handler (amarok :uri "/" :default-request-type :GET)
    ((what :parameter-type 'string))
  (let* ((metadata (amarok-metadata))
         (artist (extract-artist-from-metadata metadata))
         (title (extract-title-from-metadata metadata))
         (album (extract-album-from-metadata metadata)))
      (with-html-output-to-string (*standard-output* nil :indent t)
        (:html
         (:head
          (:meta :http-euiv "refresh" :content "60")
          (:link :rel "stylesheet" :type "text/css" :href "style.css")
          (:title "Amarok Control"))
         ;; color is not right i think
         (:body :bgcolor "white" :color "black"
                (:h1 "Amarok Control")
                (:table :border 3
                        (:tr
                         (:th "Artist")
                         (:th "Title")
                         (:th "Album"))
                        (:tr
                         (:td (str artist))
                         (:td (str title))
                         (:td (str album))))
                (:h2 "Controls")
                (:table :border 0
                        (:tr
                         (:td 
                          (:form :action "/execute" :method "get"
                                 (:input :type "submit" :value "Previous")
                                 (:input :type "hidden" :name "what" :value "prev")))
                         (:td 
                          (:form :action "/execute" :method "get"
                                 (:input :type "submit" :value "Next")
                                 (:input :type "hidden" :name "what" :value "next")))
                         (:td
                          (:form :action "/execute" :method "get"
                                 (:input :type "submit" :value "Play")
                                 (:input :type "hidden" :name "what" :value "play")))
                         (:td
                          (:form :action "/execute" :method "get"
                                 (:input :type "submit" :value "pause")
                                 (:input :type "hidden" :name "what" :value "pause")))
                         (:td
                          (:form :action "/execute" :method "get"
                                 (:input :type "submit" :value "Stop")
                                 (:input :type "hidden" :name "what" :value "stop"))))))))))

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
    
  
(define-easy-handler (execute :uri "/execute" :default-request-type :GET)
    ((what :parameter-type 'string))
  (cond
    ((equal what "pause")
     (amarok-pause))
    ((equal what "play")
     (amarok-play))
    ((equal what "next")
     (amarok-next))
    ((equal what "prev")
     (amarok-previous))
    ((equal what "stop")
     (amarok-stop)))
  (redirect "/"))
