(defpackage :amarok-control
  (:use :common-lisp :hunchentoot :cl-who :split-sequence))

(require :split-sequence)
(require :cl-ppcre)
(in-package :amarok-control)

(require :split-sequence)
(require :cl-ppcre)


;(defparameter base-url "org.kde.amarok")
(defparameter base-url "org.mpris.audacious")
;; environment getting
(defun env-kded4-pid ()
  (let ((stream (make-string-output-stream)))
      (sb-ext:run-program "/bin/pidof" '("kded4") :wait t :output stream)
      (string-trim '(#\Newline) (get-output-stream-string stream))))

(defun env-dbus-address ()
  (with-open-file (stream (concatenate 'string "/proc/" (env-kded4-pid) "/environ"))
    (let ((line (read-line stream)))
            (subseq line
                    (+ (search "DBUS_SESSION_BUS_ADDRESS" line) 25)
                    (- (search "XDG_DATA_DIRS" line) 1)))))

(defparameter *dbus-environment* (env-dbus-address))

(defun dbus-send (method &optional &key (dest base-url) (path "/Player") (print-output nil))
  (let* ((error-stream (make-string-output-stream))
         (output-stream (make-string-output-stream))
         (argument-list (list 
                         (unless (null print-output) "--print-reply")
                         "--type=method_call" 
                         (concatenate 'string "--dest=" dest) 
                         path 
                         method))
        (process (sb-ext:run-program "/usr/bin/dbus-send" 
                                     argument-list
                                     :wait t
                                     :environment (list (concatenate 'string "DBUS_SESSION_BUS_ADDRESS=" *dbus-environment*))
                                     :error error-stream
                                     :output output-stream)))
    (values
     (get-output-stream-string output-stream)
     (when (equal (sb-ext:process-exit-code process) 1)
       (get-output-stream-string error-stream)))))
     
         


;; various wrappers for convenience
(defun amarok-play ()
  (dbus-send "org.freedesktop.MediaPlayer.Play" :print-output t))

(defun amarok-pause ()
  (dbus-send "org.freedesktop.MediaPlayer.Pause" :print-output t))

(defun amarok-stop ()
  (dbus-send "org.freedesktop.MediaPlayer.Stop" :print-output t))

(defun amarok-previous ()
  (dbus-send "org.freedesktop.MediaPlayer.Prev" :print-output t))

(defun amarok-next ()
  (dbus-send "org.freedesktop.MediaPlayer.Next" :print-output t))

(defun amarok-metadata ()
  (let ((output (dbus-send "org.freedesktop.MediaPlayer.GetMetadata" :print-output t))
        (list (list)))
    (cl-ppcre:do-register-groups (first snd third fourth)
        ("dict entry\\(\\s+string\\s+\"(.*)\"\\s*variant\\s+((.*?))\\s\"(.*)\"(.*)\\s+\\)" output)
      (push (cons first fourth) list))
    (remove-if #'(lambda (x)
                   (or 
                    (search "arturl" (first x))
                    (search "comment" (first x))))
                   list)))

(defmacro extract-from-metadata (name value)
  "extracts value from metadata"
  `(defun ,name (metadata)
     (cdr (assoc ,value metadata :test #'equalp))))

(extract-from-metadata extract-artist-from-metadata "artist")
(extract-from-metadata extract-album-from-metadata "album")
(extract-from-metadata extract-title-from-metadata "title")