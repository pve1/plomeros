(in-package :plomeros)

;;;; Haiku

(defmethod word-syllables ((string string))
  (case (length string)
    (0 0)
    (t (max 1
            (/ (length (cl-ppcre:all-matches "[aeiouyåäö]+" string))
               2)))))

(defmethod word-syllables ((word follower))
  (reduce #'+ (follower-words word) :key #'word-syllables))

(defun choose-follower-haiku (word-record max-syllables)
  (let ((followers
          (remove-if-not (lambda (x)
                           (<= (reduce
                                #'+
                                (follower-words x)
                                :key #'word-syllables)
                               max-syllables))
                         (word-record-followers word-record))))
    (when followers
      (prob-choice
       followers
       :key #'follower-frequency
       :total-freq (reduce #'+ followers
                           :key #'follower-frequency)))))

(defun %generate-phrase-tokens-haiku (stats &key topic syllables)
  (labels ((try-find-first-word (syllables &optional topic)
             (or topic
                 (loop :for i :from 0 :to 100
                       :do (let ((w (pick-random-first-word stats)))
                             (if (<= (word-syllables w) syllables)
                                 (return w))))))

           (recurse (word-record syllables-remaining)
             (cond ((< syllables-remaining 0)
                    (error "Syllables < 0"))

                   ((= syllables-remaining 0)
                    nil) ;; done

                   (t (let* ((follower (or (and word-record
                                                (choose-follower-haiku
                                                 word-record
                                                 syllables-remaining))
                                           (try-find-first-word
                                            syllables-remaining)))

                             (word (if (stringp follower)
                                       follower
                                       (first (follower-words follower)))))
                        (cons word
                              (recurse (get-word-record word stats)
                                       (- syllables-remaining
                                          (word-syllables word)))))))))

    (let ((first (or topic (try-find-first-word syllables topic))))
      (cons first
            (recurse (get-word-record first stats)
                     (- syllables (word-syllables first)))))))


(defun generate-phrase-tokens-haiku (stats &key topic syllables)

  (unless syllables
    (setf syllables '(5 7 5)))

  (setf topic
        (cond ((stringp topic)
               (list topic nil nil))

              ((null topic)
               (setf topic '(nil nil nil)))

              ((listp topic)
               topic)))

  (destructuring-bind (a b c) syllables
    (destructuring-bind (aa bb cc) topic
      (append (%generate-phrase-tokens-haiku
               stats :syllables a :topic aa)
              '(",")
              (%generate-phrase-tokens-haiku
               stats :syllables b :topic bb)
              '(",")
              (%generate-phrase-tokens-haiku
               stats :syllables c :topic cc)
              '(".")))))

(defun %plomeros-say-haiku (&rest rest &key topic syllables)
  (plomeros-say
   (format-phrase-tokens
    (apply #'generate-phrase-tokens-haiku
           *plomeros-chat-stats*
           rest))))

(defprimitive haiku (&rest rest &key topic syllables)
  (apply #'%plomeros-say-haiku rest))

(defprimitive haiku-short (&rest rest)
  (apply #'%plomeros-say-haiku :syllables '(3 5 3) rest))

(defprimitive haiku-about (topic &rest rest)
  (apply #'%plomeros-say-haiku :topic topic rest))
