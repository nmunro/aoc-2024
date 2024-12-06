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
        :collect t
        :else
        :collect nil))

(defun middle-element (lst)
  (let ((len (length lst)))
    (nth (floor len 2) lst)))

(defun process-updates (updates rules)
  (loop :for update :in updates
        :if (not (numberp (position nil (check-key-rules (get-key-rules update) rules))))
        :collect (middle-element update)))

(defun part-1 (data)
    (apply #'+ (mapcar #'parse-integer (process-updates (getf data :updates) (getf data :rules)))))

(defun part-2 (data)
    nil)

(defun day5 (file)
    (let ((data (process-data (load-data file))))
        (list (part-1 data) (part-2 data))))

;; Debug
(let ((data (process-data (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day5-data.txt"))))
    (part-1 data))

;; (let ((data (process-data (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day5-demo-data.txt"))))
;;     (part-2 data))
