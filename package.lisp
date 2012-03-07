
(defpackage :plomeros
  (:use :cl :alexandria :sqlite :cl-ppcre))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-package-locks:lock-packages '(:alexandria :sqlite :cl-ppcre)))

