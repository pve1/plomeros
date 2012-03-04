
(defpackage :plomeros
  (:use :cl :alexandria :sqlite :cl-ppcre))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-package-locks:lock-packages '(:alexandria :sqlite :cl-ppcre)))

(in-package :plomeros)


;; IRC

(defvar *channel* nil)
(defvar *message* nil)
(defvar *sender* nil)
(defvar *message-instance* nil)

(defun irc-connect (&rest rest)
  (apply #'irc:connect rest))

(defun irc-disconnect (connection)
  (irc:quit connection "Exiting.")
  (irc:die connection))

(defun split-message (message)
  (cl-ppcre:split "\\n" message :sharedp t))

(defun irc-say (connection message channel)
  (dolist (x (split-message message))
    (irc:privmsg connection channel x)))

(defun irc-notice (connection message user)
  (dolist (x (split-message message))
    (irc:notice connection user x)))

(defmacro with-irc-message (irc-message &body body)
  (let ((irc-message* (gensym))
        (message (gensym))
        (channel (gensym)))
    `(let* ((,irc-message* ,irc-message))
       (destructuring-bind (,channel ,message)
           (irc:arguments ,irc-message*)
         (let* ((*sender* (irc:source ,irc-message*))
                (*message* ,message)
                (*channel* (if (member (aref ,channel 0) '(#\#))
                               ,channel
                               *sender*))
                (*message-instance* ,irc-message*))
           ,@body)))))



;; Plomeros

(defvar *plomeros* nil)
(defvar *plomeros-hooks* '(plomeros-read-hook))
(defvar *plomeros-channels* nil)
(defvar *plomeros-connect-args* nil)
(defvar *plomeros-runtime-properties* nil)

(defun set-property (prop val)
  (setf (getf *plomeros-runtime-properties* prop) val))

(defun  get-property (prop)
  (getf *plomeros-runtime-properties* prop))

(defun plomeros-hook (msg)
  (some (lambda (f) (ignore-errors
                      (with-irc-message msg
                        (funcall f msg))))
        *plomeros-hooks*))

(defun start-plomeros (&rest rest &key nickname server)
  (let ((p (apply #'irc-connect
                  (append rest *plomeros-connect-args*))))
    (doplist (key val (append *plomeros-connect-args* rest))
      (set-property key val))
    (irc:add-hook p 'irc:irc-privmsg-message 'plomeros-hook)
    (setf *plomeros* p)
    (mapc (curry #'irc:join p) *plomeros-channels*)
    (irc:read-message-loop *plomeros*)))

(defun plomeros-say (msg &optional
                     (channel *channel*)
                     (connection *plomeros*))
  (irc-say connection msg channel))

(defun plomeros-notice (msg &optional
                        (user *sender*)
                        (connection *plomeros*))
  (irc-notice connection msg user))

(defun plomeros-read-hook (msg)
  (register-groups-bind (recipient form)
      ("(.*?)[,:] *?(\\(.*\\))" *message*)
    (when (equalp recipient (get-property :nickname))
      (let* ((*read-eval* nil)
             (*package* (find-package :plomeros))
             (sexp (read-from-string form)))
        (eval-plomeros sexp)))))

(defun eval-plomeros (form)
  (format t "EVAL-PLOMEROS: ~S~%" form)
  (assert (every #'stringp (rest form)))
  (destructuring-case form
    ((update) (asdf:load-system :plomeros))
    ((say msg) (plomeros-say msg))
    ((join chan) (irc:join *plomeros* chan))
    ((part chan) (irc:part *plomeros* chan))
    ((nick new-nick)
     (irc:nick *plomeros* new-nick)
     (set-property :nickname new-nick))
    ((t &rest rest) (return-from eval-plomeros nil)))
  t)
