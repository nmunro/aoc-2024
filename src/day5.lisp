(defpackage advent-of-code-2024/day5
  (:use :cl)
  (:export #:day5))

(in-package advent-of-code-2024/day5)

(defun load-data (file)
    (with-open-file (data file :direction :input :if-does-not-exist :error)
        (loop :for line = (read-line data nil)
              :until (null line)
              :if (string-equal "" line) :collect nil
              :else :collect line)))

(defun process-data (data)
    (let ((split-point (position nil data)))
        (list :rules (subseq data 0 split-point)
              :updates (loop :for update :in (subseq data (+ 1 split-point)) :collect (cl-utilities:split-sequence #\, update)))))

(defun get-key-rules (update)
  (loop :for i :from 0 :below (length update)
        :append (loop :for j :from (+ i 1) :below (length update)
                      :collect (format nil "~A|~A" (nth i update) (nth j update)))))

(defun check-key-rules (calculated-rules rules)
  (loop :for rule :in calculated-rules
        :if (member rule rules :test #'string-equal)
        :collect rule
        :else
        :collect nil))

(defun middle-element (lst)
  (let ((len (length lst)))
    (nth (floor len 2) lst)))

(defun reverse-rule (rule)
  (let ((r (str:split "|" rule)))
    (format nil "~A|~A" (cadr r) (car r))))

(defun check-reverse-rule (rule rules)
  (let ((rrule (reverse-rule rule)))
    (if (position rrule rules :test #'string-equal)
        rrule
        nil)))

(defun get-updates (updates rules)
  (loop :for update :in updates
        :if (not (numberp (position nil (check-key-rules (get-key-rules update) rules))))
        :collect update :into valid
        :else
        :collect update :into invalid
        :finally (return (list :valid valid :invalid invalid))))

(defun reorder-invalid-update (update rules)
  (dolist (rule rules)
    (let* ((pairs (str:split "|" rule))
           (first (position (car pairs) update :test #'string-equal))
           (second (position (cadr pairs) update :test #'string-equal)))
      (when (and first second (> first second))
          (setf (aref update first) (cadr pairs))
          (setf (aref update second) (car pairs)))))
  (coerce update 'list))

(defun process-valid-updates (updates rules)
  (let ((classified-updates (get-updates updates rules)))
    (mapcar #'middle-element (getf classified-updates :valid))))

(defun process-invalid-updates (updates rules)
  (let ((classified-updates (get-updates updates rules)))
    (mapcar #'middle-element (loop :for update :in (getf classified-updates :invalid)
          :collect (let ((current-update (coerce update 'vector))
                         (previous-update nil))
                     (loop
                       :until (equal current-update previous-update)
                       :do (progn
                             (setf previous-update current-update)
                             (setf current-update (reorder-invalid-update (coerce current-update 'vector) rules))))
                     (coerce current-update 'list))))))

(defun part-1 (valid-updates)
    (apply #'+ (mapcar #'parse-integer (process-valid-updates (getf valid-updates :updates) (getf valid-updates :rules)))))

(defun part-2 (invalid-updates)
    (apply #'+ (mapcar #'parse-integer (process-invalid-updates (getf invalid-updates :updates) (getf invalid-updates :rules)))))

(defun day5 (file)
    (let ((data (process-data (load-data file))))
        (list (part-1 data) (part-2 data))))
