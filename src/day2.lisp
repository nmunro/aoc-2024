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

(defun within-safe-margin-p (x y)
  (>= 2 (abs (- x y))))

(defun get-level-pairs (report)
  (loop :for level :on report
        :while (rest level)
        :collect (list (first level) (second level))))

;;;; Unfinished
(defun is-safe-p (report)
  nil)

(defun is-increasing-p (report)
  (let* ((max-item (apply #'max report))
         (min-item (apply #'min report))
         (max-pos (find max-item report))
         (min-pos (find min-item report)))
    (format t "~A(~A) > ~A(~A): ~A -> ~A~%" max-item max-pos min-item min-pos (not (= max-item min-item)) (> max-pos min-pos))
    (and (not (= max-item min-item)) (> max-pos min-pos))))

(defun is-decreasing-p (report)
  (> (car report) (apply #'max (cdr report))))

(defun is-increasing-and-decreasing-p (report)
  (and (is-increasing-p report) (is-decreasing-p report)))


(defun day2 (file)
  (let ((data (load-data file)))
    (list nil nil)))

; min value not last element AND the max element is the last one
; There's a value higher than the min and it's position is lower than the max
;; (is-increasing-p '(1 2 3 4 5))
;; (is-increasing-p '(5 4 3 2 1))
;; (is-increasing-p '(1 2 3 2 1))

;; ; max value not last element
;; (is-decreasing-p '(1 2 3 4 5))

(get-level-pairs '(1 2 3 4 5))
