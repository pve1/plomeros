(in-package :plomeros)

(defmacro defprimitive (name args &body body)
  `(define-primitive-value ',name
       (lambda ,args
         ,@body)))

(defmacro defspecial (name args &body body)
  `(define-primitive-special-form ',name
       (lambda ,args
         ,@body)))

(define-primitive-value t t)
(define-primitive-value nil nil)

(mapc (lambda (x) (define-primitive-value x (symbol-function x)))
      *plomeros-safe-lisp-functions*)

(defspecial lambda (args &rest body)
  (make-procedure args body *env*))

;; Really let*
(defspecial let (bindings &rest rest)
  (setf bindings (mapcar (lambda (x)
                           (destructuring-bind (var form) x
                             (cons var (eval-plomeros form))))
                         bindings))
  (eval-plomeros (cons 'progn rest)
                 (extend-environment bindings *env*)))

(defspecial set (symbol value)
  (set-symbol-value symbol (eval-plomeros value) *env*))

(defspecial progn (&rest forms)
  (let ((result))
    (mapc (lambda (x) (setf result (eval-plomeros x))) forms)
    result))

(defun %cond (clauses &key (test #'identity))
  (some (lambda (x)
          (destructuring-bind (expression consequent) x
            (when (funcall test (eval-plomeros expression))
              (eval-plomeros consequent))))
        clauses))

(defspecial cond (&rest clauses)
  (%cond clauses))

(defspecial switch (thing &rest clauses)
  (let ((thing (eval-plomeros thing)))
    (%cond clauses :test (lambda (x)
                           (or (eq t x)
                               (equal x thing))))))


(defprimitive getp (list indicator)
  (getf list indicator))

(defprimitive setp (symbol indicator value)
  (let ((exist (get-symbol-value symbol)))
    (if exist
        (progn (setf (getf exist indicator) value)
               (set-symbol-value symbol exist *env*))
        (set-symbol-value symbol (list indicator value) *env*))))

(defprimitive apply (proc &rest args)
  (apply-plomeros proc args))

(defprimitive show (msg &optional (channel *channel*))
  (plomeros-say
   (prin1-to-string
    (prettify-output msg))
   channel))

(defprimitive say (msg &optional (channel *channel*))
  (plomeros-say
   (princ-to-string
    (prettify-output msg))
   channel))


(defprimitive join (chan) (irc:join *plomeros* chan))
(defprimitive part (chan) (irc:part *plomeros* chan))

(defprimitive nick (new-nick)
  (irc:nick *plomeros* new-nick)
  (set-property :nickname new-nick))

(defprimitive update ()
  (asdf:load-system :plomeros))
