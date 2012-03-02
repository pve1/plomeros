;;; -*- Mode: LISP -*-

(in-package :cl-user)

(asdf:defsystem :plomeros
 :serial t
 :components ((:file "plomeros")
              (:file "daily-set"))

 :depends-on (:cl-ppcre
              :cl-irc
              :sqlite
              :alexandria
              :cl-package-locks))
