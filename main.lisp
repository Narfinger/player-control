(in-package :cl-user)
(require :hunchentoot)
(require :cl-who)

(in-package :amarok-control)


;; (setf *dispatch-table*
;;       (list #'dispatch-easy-handlers
;;             #'default-dispatcher))

(setf *dispatch-table*
      (list #'dispatch-easy-handlers))

(push (create-static-file-dispatcher-and-handler "/style.css" "./style.css")
      *dispatch-table*)

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username "nobody") (string= password "password"))
            ,@body)
           (t (hunchentoot:require-authorization "amarok-control")))))

(defconstant amarok-controls
  (with-html-output-to-string (*standard-ouptput* nil :indent nil)
    (:table :border 0
            (:tr
             (:td 
              (:form :action "/execute" :method "get"
                     (:button :type "submit" :name "what" :value "prev" "Previous")))
             (:td 
              (:form :action "/execute" :method "get"
                     (:button :type "submit" :name "what" :value "next" "Next")))
             (:td
              (:form :action "/execute" :method "get"
                     (:button :type "submit" :name "what" :value "play" "Play")))
             (:td
              (:form :action "/execute" :method "get"
                     (:button :type "submit" :name "what" :value "pause" "Pause")))
             (:td
              (:form :action "/execute" :method "get"
                     (:button :type "submit" :name "what" :value "pp" "PlayPause")))
             (:td
              (:form :action "/execute" :method "get"
                     (:button :type "submit" :name "what" :value "stop" "Stop")))))))

(defconstant serieviewer-controls
  (with-html-output-to-string (*standard-output* nil :indent nil)
    (:table :border 0
            (:tr
             (:td
              (:form :action "/execute" :method "get"
                     (:button :type "submit" :value "playnexts" "Play Next Episode in Series")))))))

(define-easy-handler (amarok :uri "/" :default-request-type :GET)
    ((what :parameter-type 'string))
  (let* ((metadata (amarok-metadata))
         (artist (extract-artist-from-metadata metadata))
         (title (extract-title-from-metadata metadata))
         (album (extract-album-from-metadata metadata))
         (current-action (get-playstatus-string))
         (serieviewer-running (serieviewer-get-status-string))
         ;; (number (random 100))
         )
      (with-html-output-to-string (*standard-output* nil :indent t)
        (:html
         (:head
          (:meta :http-equiv "refresh" :content "60")
          ;; (:link :rel "stylesheet" :type "text/css" :href (concatenate 'string "style.css?" (write-to-string number)))
          (:link :rel "stylesheet" :type "text/css" :href "style.css" )
          (:title "Amarok Control"))
         ;; color is not right i think
         (:body :bgcolor "white" :color "black"
                (:h1 "Amarok Control")
                (:table :class "music"
                        (:tr
                         (:th "Artist")
                         (:th "Title")
                         (:th "Album"))
                        (:tr
                         (:td (str artist))
                         (:td (str title))
                         (:td (str album))))
                (:h2 "Controls")
                (str amarok-controls)
                (:p :class "status" "Status: " (str current-action))

                (:h1 "Serieviewer")
                (:p :class "status" "Status: " (str serieviewer-running))
                (str serieviewer-controls))))))

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
    ((equal what "pp")
     (amarok-playpause))
    ((equal what "next")
     (amarok-next))
    ((equal what "prev")
     (amarok-previous))
    ((equal what "stop")
     (amarok-stop))
    ((equal what "playnexts")
     (serieviewer-play)))
  (redirect "/"))
