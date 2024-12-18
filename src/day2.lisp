(defpackage advent-of-code-2024/day2
  (:use :cl)
  (:export #:day2))

(in-package advent-of-code-2024/day2)

(defun load-data (file)
  (with-open-file (in file :direction :input :if-does-not-exist :error)
    (flet ((split-line (line) (remove-if (lambda (str) (string-equal str "")) (cl-utilities:split-sequence #\Space line))))
        (loop :for raw-line = (read-line in nil)
              :until (eq raw-line nil)
              :collect (loop :for level :in (split-line raw-line) :collect (parse-integer level))))))

(defun check-safety-margin (report)
  (flet ((within-safe-margin-p (x y)
          (and (>= 3 (abs (- x y))) (not (= x y))))
         (get-level-pairs (report)
          (loop :for level :on report :while (rest level) :collect (list (first level) (second level)))))
    (let ((levels (loop :for pair :in (get-level-pairs report) :collect (within-safe-margin-p (car pair) (cadr pair)))))
      (eq nil (position nil levels)))))

(defun is-safe-p (report)
  (cond ((and (equal report (sort (copy-list report) #'<)) (check-safety-margin report)) t)
        ((and (equal report (sort (copy-list report) #'>)) (check-safety-margin report)) t)
        (t nil)))

(defun is-safe-with-one-bad-report-p (report)
  (flet ((report-permutations (report)
           (loop :for i :from 0 :below (length report) :collect (append (subseq report 0 i) (nthcdr (1+ i) report)))))
    (not (eq nil (position t (loop :for r :in (report-permutations report) :collect (is-safe-p r)))))))

(defun day2 (file)
  (let ((data (load-data file)))
    (list (length (loop :for report :in data :if (eq t (is-safe-p report)) :collect t))
          (length (loop :for report :in data :if (eq t (is-safe-with-one-bad-report-p report)) :collect t)))))
