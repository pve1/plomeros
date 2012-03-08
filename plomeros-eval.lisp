(in-package :plomeros)

;;;; Global symbol table.

(defvar *symbol-table* (make-hash-table :test 'eq))

;;;; Functions "exported" from lisp.

(defparameter *plomeros-safe-lisp-functions*
  '(cons list append reverse car cdr + - * / = equal cat random length nth))

(defun safe-function-p (symbol)
  (member symbol *plomeros-safe-lisp-functions* :test #'eq))

;;;; Primitives

(defvar *primitive-symbols* (make-hash-table :test 'eq))
(defvar *primitive-values* (make-hash-table :test 'eq))

(defun clear-primitives ()
  (setf *primitive-symbols* (make-hash-table :test 'eq))
  (setf *primitive-values* (make-hash-table :test 'eq)))

(defun lookup-primitive-symbol (symbol &optional (table *primitive-symbols*))
  (gethash symbol table))

(defun primitive-symbol-p (symbol &optional (table *primitive-symbols*))
  (lookup-primitive-symbol symbol table))

(defun primitive-value-p (value &optional (values *primitive-values*))
  (gethash value values))

(defun define-primitive-value (symbol value &optional
                               (table *primitive-symbols*)
                               (values *primitive-values*))
  (remhash (gethash symbol table) values)
  (setf (gethash symbol table) value)
  (setf (gethash value values) symbol))

(define-primitive-value nil nil)

(define-primitive-value 'getp
    (lambda (list indicator)
      (getf list indicator)))

(define-primitive-value 'setp
    (lambda (symbol indicator value)
      (let ((exist (get-symbol-value symbol)))
        (if exist
            (progn (setf (getf exist indicator) value)
                   (set-symbol-value symbol exist *env*))
            (set-symbol-value symbol (list indicator value) *env*)))))


;;;; Util

(defun cat (&rest args)
  (apply #'concatenate 'string args))

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

(defun plomeros-read-hook (msg)
  (register-groups-bind (recipient form)
      ("(.*?)[,:] *?(\\(.*\\))" *message*)
    (when (equalp recipient (get-property :nickname))
      (let* ((*read-eval* nil)
             (*package* (find-package :plomeros))
             (sexp (read-from-string form)))
        (eval-plomeros sexp)
        t))))


;;;; Lambda

(defun make-procedure (args body env)
  `(lambda ,args ,env ,@body))

(defun procedure-arguments (proc)
  (second proc))

(defun procedure-environment (proc)
  (third proc))

(defun procedure-body (body)
  (nthcdr 3 body))

(defun procedurep (thing)
  (eq (car thing) 'lambda))


;;;; Eval

(defvar *eval-depth* 0)

(defun check-eval-depth ()
  (unless (zerop *eval-depth*)
    (plomeros-say "Recursion is for noobs.")
    (error "Stack blown.")))

(defun %cond (clauses &key (test #'identity))
  (some (lambda (x)
          (destructuring-bind (expression consequent) x
            (when (funcall test (eval-plomeros expression))
              (eval-plomeros consequent))))
        clauses))

(defun prettify-output (thing)
  (when thing
    (typecase thing
      (function '#:PRIMITIVE-FUNCTION)
      (list (mapcar #'prettify-output thing))
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
  (format t "EVAL-PLOMEROS: ~S~%" form)
  (cond ((null form) nil)

        ((keywordp form)
         form)

        ((symbolp form)
         (get-symbol-value form *env*))

        ((atom form)
         form)

        ((listp form)
         (destructuring-case form
           ((quote quoted-form) quoted-form)

           ((set symbol value)
            (set-symbol-value symbol (eval-plomeros value) *env*))

           ((update) (asdf:load-system :plomeros))

           ((show msg &optional (channel *channel*))
            (plomeros-say
             (prin1-to-string
              (prettify-output
               (eval-plomeros msg)))
             (eval-plomeros channel)) )

           ((say msg &optional (channel *channel*))
            (plomeros-say
             (princ-to-string
              (prettify-output
               (eval-plomeros msg)))
             (eval-plomeros channel)))

           ((join chan) (irc:join *plomeros* (eval-plomeros chan)))
           ((part chan) (irc:part *plomeros* (eval-plomeros chan)))

           ((nick new-nick)
            (let ((%new-nick (eval-plomeros new-nick)))
              (irc:nick *plomeros* %new-nick)
              (set-property :nickname %new-nick)))

           ((lambda args &rest body)
            (make-procedure args body *env*))

           ((apply proc &rest args)
            (apply-plomeros (eval-plomeros proc)
                            (mapcar #'eval-plomeros args)))

           #+nil
           ((eval form)
            (check-eval-depth)
            (let* ((*eval-depth* (+ *eval-depth* 1))
                   (%form (eval-plomeros form)))
              (eval-plomeros %form)))

           ((progn &rest forms)
            (let ((result))
              (mapc (lambda (x) (setf result (eval-plomeros x))) forms)
              result))

           ((cond &rest clauses)
            (%cond clauses))

           ((switch thing &rest clauses)
            (let ((thing (eval-plomeros thing)))
              (%cond clauses :test (lambda (x) (equal x thing)))))

           ((t &rest rest)
            (let* ((op (car form))
                   (eval-op (eval-plomeros op)))

              (cond ((safe-function-p op)
                     (apply (symbol-function op)
                            (mapcar #'eval-plomeros rest)))

                    ((primitive-value-p eval-op)
                     (apply eval-op
                            (mapcar #'eval-plomeros rest)))

                    ((procedurep eval-op)
                     (apply-plomeros eval-op (mapcar #'eval-plomeros rest))))))))))
