;;; -*- Mode: LISP -*-

(in-package :cl-user)

(asdf:defsystem :plomeros
 :serial t
 :components ((:file "package")
              (:file "plomeros")
              (:file "plomeros-eval")
              (:file "plomeros-primitives")
              (:file "daily-set")
              (:file "chat")
              (:file "response")
              (:file "haiku"))

 :depends-on (:cl-ppcre
              :cl-irc
              :sqlite
              :alexandria
              :cl-package-locks))
