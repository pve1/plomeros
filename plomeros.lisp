
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
  (some (lambda (f)
          (multiple-value-bind (value error)
              (ignore-errors
                (with-irc-message msg (funcall f msg)))
            (when error
              (format t "~A~%" error))
            value))
        *plomeros-hooks*))

(defun plomeros-read-message-loop (&key (connection *plomeros*)
                                   (retry-interval 300)
                                   max-retries)
  (labels ((%message-loop (&optional retries)
             (handler-case (irc:read-message-loop connection)
               (usocket:socket-error (x)
                 (progn (format t "Caught ~A~%" x)
                        (format t "Sleeping for ~A seconds.~%"
                                retry-interval)
                        (sleep retry-interval))))
             (unless (and retries (zerop retries))
               (%message-loop (when retries (1- retries))))))
    (%message-loop max-retries)))

(defun start-plomeros (&rest rest &key nickname server)
  (let ((p (apply #'irc-connect
                  (append rest *plomeros-connect-args*))))
    (setf *plomeros* p)
    (doplist (key val (append *plomeros-connect-args* rest))
      (set-property key val))
    (irc:add-hook p 'irc:irc-privmsg-message 'plomeros-hook)
    (mapc (curry #'irc:join p) *plomeros-channels*)
    (plomeros-read-message-loop)))

(defun plomeros-say (msg &optional
                     (channel *channel*)
                     (connection *plomeros*))
  (irc-say connection msg channel))

(defun plomeros-notice (msg &optional
                        (user *sender*)
                        (connection *plomeros*))
  (irc-notice connection msg user))


