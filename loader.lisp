(require :asdf)
(pushnew "amarok-control" asdf:*central-registry* :test #'equal)

(asdf:load-system :amarok-control)

(in-package :amarok-control)

(defconstant server-port 8080)
(defconstant server-address "192.168.24.1")

(defvar *ht-server* (hunchentoot:start-server :port server-port :address server-address))
