(in-package :plomeros)

(defparameter *responses* nil)

(defstruct (response (:constructor %make-response)) regex fn)

(defun make-response (regex plomeros-function)
  (%make-response
   :regex regex
   :fn (lambda (msg-string)
         (multiple-value-bind (match subs)
             (cl-ppcre:scan-to-strings regex msg-string)
           (when match
             (let ((subs-list (coerce subs 'list)))
               (eval-plomeros (cons plomeros-function subs-list))
               t))))))

(defun apply-response (fn thing)
  (funcall fn thing
           *responses*
           :test #'string-equal
           :key #'response-regex))

(defun response-exists-p (response)
  (apply-response #'position (response-regex response)))

(defun add-response (response)
  (if-let ((index (response-exists-p response)))
    (setf (nth index *responses*) response)
    (push response *responses*)))

(defun remove-response (regex)
  (setf *responses* (apply-response #'delete regex)))

(defun match-response (response msg)
  (funcall (response-fn response) msg))

(defun response-hook (msg)
  (declare (ignore msg))
  (some (lambda (r)
          (match-response r *message*))
        *responses*))

(defprimitive add-response (regex function)
  (add-response (make-response regex function)))

(defprimitive remove-response (regex)
  (cond ((numberp regex)
         (when-let ((r (nth regex *responses*)))
           (remove-response (response-regex r))))
        (t (remove-response regex))))

(defprimitive list-responses ()
  (plomeros-say
   (prin1-to-string
    (mappend (lambda (i r)
               (list i (response-regex r)))
             (iota (length *responses*))
             *responses*))))

(unless (member 'response-hook *plomeros-hooks*)
  (setf *plomeros-hooks* (append *plomeros-hooks*
                                 '(response-hook))))
