(in-package :plomeros)

(let ((f (complement #'alpha-char-p)))
  (defun split-line (line)
    (split-sequence:split-sequence-if
     f
     line
     :remove-empty-subseqs t)))

(defun make-empty-chat-stats ()
  (make-hash-table :test 'equal))

(defstruct word-record
  (frequency 0)
  (followers nil)
  (follower-total-freq 0))

(defstruct follower (frequency 0) (words nil))

(defun make-empty-chat-lexicon ()
  (make-hash-table :test #'equal))

(defvar *chat-lexicon* (make-empty-chat-lexicon))

(defun intern-word (word)
  (setf word (string-downcase word))
  (let ((w (gethash word *chat-lexicon*)))
    (or w (setf (gethash word *chat-lexicon*) word))))

(defun record-word (word follower stats)
  (assert follower)
  (setf word (intern-word word)
        follower (mapcar #'intern-word follower))
  (let ((rec (gethash word stats)))
    (unless rec
      (setf rec (make-word-record))
      (setf (gethash word stats) rec))
     (incf (word-record-frequency rec))
     (let ((fol (find follower (word-record-followers rec)
                      :test #'equal
                      :key #'follower-words)))
       (if fol
           (incf (follower-frequency fol))
           (push (make-follower :words follower :frequency 1)
                 (word-record-followers rec))))))

(defun record-tokens (stats tokens n)
  (loop :for words = tokens :then (cdr words)
     :for sub = (subseq words 0 (min n (length words)))
     :while (< 1 (length sub))
     :do (record-word (first sub)
                      (rest sub)
                      stats)))

(defun get-word-record (word stats)
  (gethash word stats))

(defun do-word-records (stats fn)
  (maphash fn stats))

(defun word-records-count (stats)
  (hash-table-count stats))

(defun finalize-word-counts (stats)
  (maphash (lambda (k v)
             (if (= 1 (length (word-record-followers v)))
                 (remhash k stats)
                 (setf (word-record-followers v)
                       (sort (word-record-followers v)
                             #'> :key #'follower-frequency))))
           stats)
  stats)

(defun prob-choice (list &key (key #'cdr) total-freq)
  (let ((rnd (random (or total-freq
                         (reduce #'+ list :key key)))))
    (loop :for el :in list
       :for f = (funcall key el) :then (+ f (funcall key el))
       :when (< rnd f)
       :return el)))

(defun choose-follower (word-record)
  (prob-choice (word-record-followers word-record)
               :key #'follower-frequency
               :total-freq (word-record-frequency word-record)))

(defun random-follower (word-record)
  (nth (random (length (word-record-followers word-record)))
       (word-record-followers word-record)))

(defun compact-word-record (rec)
  (mapc (lambda (fol)
          (setf (follower-words fol)
                (mapcar #'intern-word (follower-words fol))))
        (word-record-followers rec))
  rec)

(defun read-plomeros-chat-file (file)
  (let ((st (make-empty-chat-stats))
        (*package* (find-package :plomeros)))
    (with-open-file (s file)
      (loop :for entry = (read s nil)
         :while entry
         :do (setf (gethash (intern-word (car entry)) st)
                   (compact-word-record (cdr entry)))))
    st))

(defun serialize-stats (stats stream)
  (maphash (lambda (k v)
             (print (cons k v) stream))
           stats))


;;;; Parsing of input

(defun %make-line-generator (stream)
  (lambda () (read-line stream nil)))

(defun %ensure-line-generator (thing)
  (etypecase thing
    (function thing)
    (stream (%make-line-generator thing))))

(defmacro do-while-lines ((line-var generator) &body body)
  `(loop :for ,line-var = (funcall ,generator)
      :while ,line-var
      :do ,@body))

(defun make-line-generator (stream tokenize-function)
  (let ((s (%ensure-line-generator stream)))
    (lambda ()
      (do-while-lines (line s)
        (when-let ((tokens (funcall tokenize-function line)))
          (return tokens))))))

;; Normal text

(defun make-text-line-generator (stream)
  (make-line-generator stream #'split-line))

;; Irssi format

(defun tokenize-irssi-line (line &key ignore-users)
  (cl-ppcre:register-groups-bind (user msg)
      ("^\\d\\d:\\d\\d < *([^ ]*?)> (.*)" line)
    (unless (member user ignore-users :test #'equalp)
      (when-let ((tokens (split-line msg)))
        tokens))))

(defun make-irssi-line-generator (stream &key ignore-users)
  (make-line-generator
   stream
   (rcurry #'tokenize-irssi-line :ignore-users ignore-users)))

(defun ensure-line-generator (thing)
  (etypecase thing
    (function thing)
    (stream (make-text-line-generator thing))))

;; Stream should be line token generator of file stream.

(defun compile-stats-n (n stream)
  (let ((stats (make-empty-chat-stats))
        (gen (ensure-line-generator stream)))
    (loop :for line = (funcall gen)
       :while line
       :do (record-tokens stats line n))
    (finalize-word-counts stats)))

(defun shell-compile-stats-for-file (in-file out-file n &key format ignore-users)
  (with-open-file (in in-file)
    (let* ((in-gen (case format
                     (:irssi (make-irssi-line-generator
                              in :ignore-users ignore-users))
                     (t  (make-text-line-generator in))))
           (stats (compile-stats-n n in-gen)))
      (with-output-to-file (out out-file)
        (serialize-stats stats out)))))


(defvar *plomeros-chat-stats* (make-empty-chat-stats))

(defun init-plomeros-chat (file)
  (setf *plomeros-chat-stats* (read-plomeros-chat-file file))
  nil)

(defun pick-random-first-word (stats)
  (let* ((nth-word (random (word-records-count stats))))
    (do-word-records stats
      (lambda (w f)
        (if (zerop nth-word)
            (return-from pick-random-first-word w)
            (decf nth-word))))))

(defvar *punctuation* '(("," 20 :space after :eos nil :cap nil)
                        ("." 10 :space after :eos t :cap t)
                        ("?" 3 :space after :eos t :cap t)
                        (" " 3 :space nil :eos nil :cap nil)
                        ("!" 2 :space after :eos t :cap t)
                        (":)" 2 :space before-after :eos t :cap t)))

(defun generate-phrase-tokens (stats &key (n (+ 5 (random 15)))
                               (follower-func #'choose-follower)
                               topic)
  (let* ((first-word (or topic (pick-random-first-word stats)))
         (words))

    (push first-word words)

    (loop :with word = first-word
       :for k :from 0 to n
       :for record = (get-word-record word stats)
       :for follower = (when (and record (word-record-followers record))
                         (funcall follower-func record))
       :do (if follower
               (setf word (car (last (follower-words follower))))
               (setf word (pick-random-first-word stats)))

       :do (if follower
               (dolist (w (follower-words follower))
                 (push w words))
               (push (car (prob-choice *punctuation* :key #'second)) words)))

    (nreverse words)))

(defun format-phrase-tokens (tokens)
  (labels ((punctuationp (x)
             (member x *punctuation* :key #'car :test #'equal))
           (punctuation-prop (x prop)
             (getf (find x *punctuation* :test #'equal :key #'first) prop))
           (punctuation-space (x)
             (punctuation-prop x :space))
           (punctuation-cap (x)
             (punctuation-prop x :cap))
           (punctuation-eos (x)
             (punctuation-prop x :eos)))

    (with-output-to-string (s)
      (loop :with cap = t
         :for (a b) :on tokens :by #'cdr
         :for punctuation-a = (punctuationp a)
         :for punctuation-b = (punctuationp b)
         :do
         (when cap
           (setf a (string-capitalize a))
           (setf cap nil))
         (cond ((and punctuation-a
                     (not b))
                (format s "~A" (case (punctuation-eos a)
                                 ((nil) ".")
                                 (t a))))

               (punctuation-a
                (let ((space (punctuation-space a)))
                  (when (punctuation-cap a)
                    (setf cap t))
                  (format s "~A~A"
                          a
                          (case space
                            ((after before-after)  " ")
                            (t "")))))

               ((not b)
                (format s "~A." a))

               (t (format s "~A~A"
                          a
                          (if punctuation-b
                              (case (punctuation-space b)
                                ((before before-after) " ")
                                (t ""))
                              " "))))))))

(defun generate-phrase (stats &key (n 10) (mood :normal) topic)
  (format-phrase-tokens
   (case mood
     (:crazy (generate-phrase-tokens stats :n n :follower-func #'random-follower :topic topic))
     (:normal (generate-phrase-tokens stats :n n :topic topic)))))


(defun %plomeros-chat (&rest rest &key n mood topic)
  (plomeros-say (apply #'generate-phrase *plomeros-chat-stats* rest)))

(defprimitive chat (&rest rest)
  (apply #'%plomeros-chat rest))

(defprimitive chat-about (topic &rest rest)
  (apply #'%plomeros-chat :topic topic rest))

(defprimitive chat-crazy (&rest rest)
  (apply #'%plomeros-chat :mood :crazy rest))

(defprimitive chat-crazy-about (topic &rest rest)
  (apply #'%plomeros-chat :mood :crazy :topic topic rest))

(defprimitive chatn (n &rest rest &key mood)
  (apply #'%plomeros-chat :n n rest))

 
