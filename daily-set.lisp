
(in-package :plomeros)

;;;; Db

(defvar *daily-set-db* nil) ;; Needs to be set or bound for triggers

(defun call-with-daily-set-db (db fn)
  (sqlite:with-open-database (*daily-set-db* db)
    (funcall fn)))

(defmacro with-daily-set-db (db &body body)
  `(call-with-daily-set-db ,db (lambda () ,@body)))

(defun daily-set-query (query &rest parameters)
  (apply #'sqlite:execute-to-list *daily-set-db* query parameters))

(defun daily-set-single-query (query &rest parameters)
  (apply #'sqlite:execute-single *daily-set-db* query parameters))

(defun daily-set-non-query (query &rest parameters)
  (apply #'sqlite:execute-non-query *daily-set-db* query parameters))



;;;; Dates and time

(defun last-week ()
  (- (get-universal-time)
     (* 7 24 60 60)))

(defun last-month ()
  (- (get-universal-time)
     (* 30 24 60 60)))

(macrolet ((start-of (what)
             `(let ((now (get-universal-time)))
                (multiple-value-bind
                      (sec min hou date month year day dst tz)
                    (decode-universal-time now)
                  (declare (ignore month year dst tz))
                  day date ;; "use" them
                  (let ((time (- now
                                 ,what
                                 sec
                                 (* 60 min)
                                 (* 60 60 hou))))
                    time)))))

  (defun start-of-month ()
    (start-of (* (1- date) 24 60 60)))

  (defun start-of-week ()
    (start-of (* day 24 60 60))))

(defun universal-time-to-string (time)
  (multiple-value-bind (sec min hou day mon year q w e)
      (decode-universal-time time)
   (declare (ignore sec q w e))
   (format nil "~A-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
            year mon day hou min)))

(defun universal-time-to-date (time)
  (multiple-value-bind (sec min hou day mon year q w e)
      (decode-universal-time time)
    (declare (ignore sec q w e min hou))
    (format nil "~A-~2,'0D-~2,'0D"
            year mon day)))

(defun seconds-to-string (seconds &key (type :brief))
  (let ((sec (round (mod seconds 60)))
        (min (floor (/ seconds 60))))
    (case type
    (:brief
     (format nil "~A:~2,'0D" min sec))
    (:verbose
     (format nil "~A~A seconds"
             (if (zerop min)
                 ""
                 (format nil "~A minutes, " min))
             sec)))))



;;;; Daily set

(defun plomeros-daily-set-hook (msg)
  (declare (ignore msg))
  (let ((parsed (parse-daily-set-result *message*)))
    (when parsed
      (with-daily-set-db (get-property :daily-set-db)
        (update-daily-set-stats *sender* parsed)
        (plomeros-notice
         (get-daily-set-stats *sender*)
         *sender*)))))

(defun update-daily-set-stats (name seconds &key
                               (date (get-universal-time))
                               (minutes 0))
  (when (and name seconds date)
    (daily-set-non-query
     "insert into daily_set_completion (name, date, completion_time) values (?, ?, ?);"
     name date (+ (* minutes 60) seconds))))

(defun get-daily-set-stats (name)
  (if (daily-set-name-exists-p name)
      (progn (format nil "Daily set stats for ~A:~%~A~%~A"
                     name
                     (get-daily-set-avg name)
                     (get-daily-set-record name)))
      (format nil "No stats for ~A." name)))

(defun daily-set-name-exists-p (name)
  (daily-set-single-query "select name from daily_set_completion where name = ?" name))

(defun get-aliases-for-name (name)
  (let ((a (daily-set-query "select alias from daily_set_nickname_alias where name = ?" name)))
      (mapcar #'first a)))

(defun parse-integer-maybe (thing)
  (typecase thing
    (string (parse-integer thing))
    (number thing)))

(defun parse-daily-set-result (string)
  (or (register-groups-bind (mins secs)
          (" *\\[Today's puzzle completed (\\d+) Mins?, (\\d+) Secs? *\\]" string)
        (ignore-errors (+ (* 60 (parse-integer-maybe mins)) (parse-integer-maybe secs))))
      (register-groups-bind (secs)
          (" *\\[Today's puzzle completed.* (\\d+) Secs? *\\]" string)
        (ignore-errors (parse-integer-maybe secs)))))

(defun replicate (x n)
  (loop :for i :from 1 :to n :collect x))

(defun %make-sqlite-var-list (length-or-list)
  (etypecase length-or-list
    (integer (replicate "?" length-or-list))
    (list (%make-sqlite-var-list (length length-or-list)))))

(defun make-sqlite-var-list (list)
  (let ((vars (%make-sqlite-var-list list)))
    (format nil "~A~{, ~A~}" (first vars) (rest vars))))

(defun get-daily-set-avg (name)
  (let ((aliases (get-aliases-for-name name)))
    (labels ((get-avg (&optional (date-range 0))
               (apply #'daily-set-query
                      (format nil "select avg(completion_time) from daily_set_completion where name in (~A) and date > ?"
                              (make-sqlite-var-list (1+ (length aliases))))
                      (append (list name) aliases (list date-range))))

             (show-avg (avg)
               (if avg
                   (seconds-to-string
                    (parse-integer-maybe avg))
                   "N/A")))

      (let* ((weekly (caar (get-avg (start-of-week))))
             (monthly (caar (get-avg (start-of-month))))
             (all-time (caar (get-avg))))

        (when all-time
          (format nil "Weekly average: ~A, monthly average: ~A, all-time average: ~A"
                  (show-avg weekly)
                  (show-avg monthly)
                  (show-avg all-time)))))))


(defun get-daily-set-record (name)
  (let ((aliases (get-aliases-for-name name)))
    (labels ((get-rec (&optional (date-range 0))
               (let ((result
                      (apply #'daily-set-query
                             (format nil "select completion_time, date from daily_set_completion where name in (~A) and date > ? order by completion_time asc limit 1"
                                     (make-sqlite-var-list (1+ (length aliases))))
                             (append (list name) aliases (list date-range)))))
                 (first result)))

             (format-record (time date)
               (format nil "~A at ~A"
                       (seconds-to-string time)
                       (universal-time-to-string date))))

      (let ((weekly-rec (get-rec (start-of-week)))
            (monthly-rec (get-rec (start-of-month)))
            (all-time (get-rec)))

        (if all-time
            (format nil "Weekly record: ~A, monthly record: ~A, all-time record: ~A."
                    (if weekly-rec
                        (apply #'format-record weekly-rec)
                        "N/A")
                    (if monthly-rec
                        (apply #'format-record monthly-rec)
                        "N/A")
                    (apply #'format-record all-time))
            (format nil "No record found for ~A." name))))))
