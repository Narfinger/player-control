(defpackage :amarok-control
  (:use :common-lisp :hunchentoot :cl-who))

(in-package :amarok-control)


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

(defun dbus-send (method &optional &key (dest "org.kde.amarok") (path "/Player"))
  (let* ((error-stream (make-string-output-stream))
         (argument-list (list "--type=method_call" (concatenate 'string "--dest=" dest) path method))
        (process (sb-ext:run-program "/usr/bin/dbus-send" 
                                     argument-list
                                     :wait t
                                     :environment (list (concatenate 'string "DBUS_SESSION_BUS_ADDRESS=" *dbus-environment*))
                                     :error error-stream)))
    (when (equal (sb-ext:process-exit-code process) 1)
      (get-output-stream-string error-stream))))


;; various wrappers for convenience
(defun amarok-playpause ()
  (dbus-send "org.freedesktop.MediaPlayer.PlayPause"))

(defun amarok-stop ()
  (dbus-send "org.freedesktop.MediaPlayer.Stop"))

(defun amarok-previous ()
  (dbus-send "org.freedesktop.MediaPlayer.Prev"))

(defun amarok-next ()
  (dbus-send "org.freedesktop.MediaPlayer.Next"))

(defun amarok-metadata ()
  (dbus-send "GetMetadata"))