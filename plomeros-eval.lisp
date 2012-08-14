(in-package :plomeros)

;;;; Global symbol table.

(defvar *symbol-table* (make-hash-table :test 'eq))

;;;; Functions "exported" from lisp.

(defparameter *plomeros-safe-lisp-functions*
  '(cons list append reverse car cdr + - * / = equal cat random length nth null zerop))

(defparameter *plomeros-safe-lisp-specials*
  '(*sender* *message* *channel*))

(defun safe-function-p (function)
  (typecase function
    (symbol (member function *plomeros-safe-lisp-functions* :test #'eq))
    (function (primitive-value-p function))))

(defun safe-special-p (symbol)
  (member symbol *plomeros-safe-lisp-specials* :test #'eq))


;;;; Bindings

(defun binding-symbol (binding)
  (car binding))

(defun binding-value (binding)
  (cdr binding))

(defun (setf binding-value) (new-value binding)
  (setf (cdr binding) new-value))

;;;; Enviroments

(defun empty-environment ()
  nil)

(defvar *env* (empty-environment))

(defun extend-environment (bindings env)
  (cons bindings env))

(defun lookup-binding-in-environment (symbol env)
  (let ((binding))
    (find-if (lambda (frame)
               (setf binding (find symbol frame :key #'binding-symbol)))
             env)
    binding))

(defun modify-binding-in-environment (symbol value env)
  (let ((binding (lookup-binding-in-environment symbol env)))
    (when binding
      (setf (binding-value binding) value)
      t)))


;;;; Primitives

(defvar *primitive-special-forms* (make-hash-table :test 'eq))
(defvar *primitive-symbols* (make-hash-table :test 'eq))
(defvar *primitive-values* (make-hash-table :test 'eq))

(defun clear-primitives ()
  (setf *primitive-special-forms* (make-hash-table :test 'eq))
  (setf *primitive-symbols* (make-hash-table :test 'eq))
  (setf *primitive-values* (make-hash-table :test 'eq)))

(defun lookup-primitive-symbol (symbol &optional (table *primitive-symbols*))
  (gethash symbol table))

(defun primitive-symbol-p (symbol &optional (table *primitive-symbols*))
  (lookup-primitive-symbol symbol table))

(defun lookup-primitive-special-form (symbol &optional (table *primitive-special-forms*))
  (gethash symbol table))

(defun primitive-special-form-p (symbol &optional (table *primitive-special-forms*))
  (lookup-primitive-special-form symbol table))

(defun primitive-value-p (value &optional (values *primitive-values*))
  (gethash value values))

(defun define-primitive-value (symbol value &optional
                               (table *primitive-symbols*)
                               (values *primitive-values*))
  (remhash (gethash symbol table) values)
  (setf (gethash symbol table) value)
  (setf (gethash value values) symbol))

(defun define-primitive-special-form (symbol value &optional
                                      (table *primitive-special-forms*)
                                      (values *primitive-values*))
  (define-primitive-value symbol value table values))

(defun define-special-form (symbol value &optional
                            (table *primitive-symbols*)
                            (values *primitive-values*))
  (remhash (gethash symbol table) values)
  (setf (gethash symbol table) value)
  (setf (gethash value values) symbol))



;;;; Util

(defun cat (&rest args)
  (apply #'concatenate 'string args))


;;;; Global symbol table + (lexical) enviroment.

(defun set-symbol-value (symbol value &optional env (symbol-table *symbol-table*))
  (when symbol
    (or (modify-binding-in-environment symbol value env)
        (setf (gethash symbol symbol-table) value))))

(defun get-symbol-value (symbol &optional env (symbol-table *symbol-table*))
  (or (binding-value (lookup-binding-in-environment symbol env))
      (lookup-primitive-symbol symbol)
      (gethash symbol symbol-table)))


;;;; Hook

(defvar *plomeros-readtable* (copy-readtable))

(set-macro-character #\# (lambda (x y)
                           (error "Sharpsign not supported."))
                     nil *plomeros-readtable*)

(defun plomeros-read-hook (msg)
  (register-groups-bind (recipient form)
      ("(.*?)[,:] *?(\\(.*\\))" *message*)
    (when (equalp recipient (get-property :nickname))
      (let* ((*read-eval* nil)
             (*readtable* *plomeros-readtable*)
             (*package* (find-package :plomeros))
             (sexp (read-from-string form)))
        (eval-plomeros sexp)
        t))))


;;;; Lambda


(defun make-procedure (args body env)
  `(lambda-proc ,args ,env ,@body))

(defun procedure-arguments (proc)
  (second proc))

(defun procedure-environment (proc)
  (third proc))

(defun procedure-body (body)
  (nthcdr 3 body))

(defun procedurep (thing)
  (and (listp thing)
       (eq (car thing) 'lambda-proc)))


;;;; Eval

(defvar *eval-depth* 0)

(defun check-eval-depth ()
  (unless (zerop *eval-depth*)
    (plomeros-say "Recursion is for noobs.")
    (error "Stack blown.")))

(defun prettify-output (thing)
  (when thing
    (typecase thing
      (function '#:PRIMITIVE-FUNCTION)
      (list (cons (prettify-output (car thing))
                  (prettify-output (cdr thing))))
      (t thing))))

(defun apply-plomeros (proc args)
  (check-eval-depth)
  (let* ((*eval-depth* (+ *eval-depth* 1))
         (*env* (extend-environment
                 (mapcar (lambda (var arg)
                           (cons var arg))
                         (procedure-arguments proc)
                         args)
                 (procedure-environment proc))))
    (eval-plomeros (cons 'progn (procedure-body proc)))))

(defun eval-plomeros (form &optional (*env* *env*))
  (let ((result))
    (format t "EVAL-PLOMEROS: ~S~%" form)
    (setf result
          (cond ((null form) nil)

                ((keywordp form)
                 form)

                ((symbolp form)
                 (get-symbol-value form *env*))

                ((atom form)
                 form)

                ((procedurep form)
                 form)

                ((listp form)
                 (destructuring-case form
                   ((quote quoted-form) quoted-form)

                   ((update) (asdf:load-system :plomeros))

                   ((t &rest rest)
                    (let ((op (car form)))
                      (cond ((primitive-special-form-p op)
                             (multiple-value-bind (form eval?)
                                 (apply (lookup-primitive-special-form op)
                                        rest)
                               (if eval?
                                   (eval-plomeros form)
                                   form)))

                            (t (let ((actual-op (eval-plomeros op)))

                                 (cond ((primitive-value-p actual-op)
                                        (apply
                                         actual-op
                                         (mapcar #'eval-plomeros rest)))

                                       ((procedurep actual-op)
                                        (apply-plomeros
                                         actual-op
                                         (mapcar #'eval-plomeros rest)))))))))))))
    (format t "-> ~S~%" result)
    result))
