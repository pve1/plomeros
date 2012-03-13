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

(defun record-word (word follower stats)
  (assert follower)
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

(defun read-plomeros-chat-file (file)
  (let ((st (make-empty-chat-stats))
        (*package* (find-package :plomeros)))
    (with-open-file (s file)
      (loop :for entry = (read s nil)
         :while entry
         :do (setf (gethash (car entry) st) (cdr entry))))
    st))


(defun serialize-stats (stats stream)
  (maphash (lambda (k v)
             (print (cons k v) stream))
           stats))
;;;;;;


(defun compile-stats-n (n stream)
  (let ((stats (make-empty-chat-stats)))
    (loop :for line = (read-line stream nil)
       :while line
       :do (loop :for words = (split-line line) :then (cdr words)
              :for sub = (subseq words 0 (min n (length words)))
              :while (< 1 (length sub))
              :do (record-word (first sub)
                               (rest sub)
                               stats)))
    (finalize-word-counts stats)))


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

(defun generate-phrase (stats &optional (n (+ 5 (random 15))))
  (let* ((first-word (pick-random-first-word stats))
         (punctuation '(("," 20 :space after :eos nil :cap nil)
                        ("." 10 :space after :eos t :cap t)
                        ("?" 3 :space after :eos t :cap t)
                        (" " 3 :space nil :eos nil :cap nil)
                        ("!" 2 :space after :eos t :cap t)
                        (":)" 2 :space before-after :eos t :cap t)))
         (words))

    (push (string-capitalize first-word) words)

    (loop :with word = first-word
       :for k :from 0 to n
       :for record = (alexandria:when-let (a (get-word-record word stats)) a)
       :for follower = (when (and record (word-record-followers record))
                         (choose-follower record))
       :do (if follower
               (setf word (car (last (follower-words follower))))
               (setf word (pick-random-first-word stats)))

       :do (if follower
               (dolist (w (follower-words follower))
                 (push (string-downcase w) words))
               (push (car (prob-choice punctuation :key #'second)) words)))

    (setf words (nreverse words))

    (labels ((punctuationp (x)
               (member x punctuation :key #'car :test #'equal))
             (punctuation-prop (x prop)
               (getf (find x punctuation :test #'equal :key #'first) prop))
             (punctuation-space (x)
               (punctuation-prop x :space))
             (punctuation-cap (x)
               (punctuation-prop x :cap))
             (punctuation-eos (x)
               (punctuation-prop x :eos)))

      (with-output-to-string (s)
        (loop :with cap = nil
           :for (a b) :on words :by #'cdr
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
                                " ")))))))))

(defprimitive chat (&optional n) (plomeros-say (generate-phrase *plomeros-chat-stats*)))
(defprimitive chatn (n) (plomeros-say (generate-phrase *plomeros-chat-stats* n)))