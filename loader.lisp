(require :asdf)
(pushnew "amarok-control" asdf:*central-registry* :test #'equal)

(asdf:load-system :amarok-control)

(in-package :amarok-control)

(defvar *ht-server* (hunchentoot:start-server :port 8080))
