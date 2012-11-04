(require :asdf)
(pushnew "amarok-control" asdf:*central-registry* :test #'equal)


;(asdf:load-system :amarok-control)
(require :amarok-control)

(in-package :amarok-control)

(defconstant server-port 8080)
(defconstant server-address "192.168.24.1")

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port server-port))
;(defvar *ht-server* (hunchentoot:start-server :port server-port :address server-address))
