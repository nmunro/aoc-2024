(defpackage advent-of-code-2024/day1
  (:use :cl)
  (:export #:day1))

(in-package advent-of-code-2024/day1)

(defun get-lists (file)
    (with-open-file (data file :direction :input :if-does-not-exist :error)
      (flet ((split-line (line) (remove-if (lambda (str) (string-equal str "")) (cl-utilities:split-sequence #\Space line))))
        (loop :for raw-line = (read-line data nil)
              :while raw-line
              :for line = (split-line raw-line)
                    :collect (parse-integer (car line)) :into list1
                    :collect (parse-integer (cadr line)) :into list2
              :finally (return (list list1 list2))))))

(defun sort-lists (lists)
    (list (sort (car lists) '<) (sort (cadr lists) '<)))

(defun total (data)
    (apply #'+ data))

(defun get-difference (lists)
    (loop :for num1 :in (car lists) :for num2 :in (cadr lists) :collect (abs (- num2 num1))))

(defun get-similarity (lists)
    (flet ((process-line (x y) (abs (* x y))))
        (loop :for num :in (car lists) :collect (process-line num (count-if (lambda (x) (= x num)) (cadr lists))))))

(defun day1 (file)
    (list (arrows:-> (get-lists file) sort-lists get-difference total)
          (arrows:-> (get-lists file) sort-lists get-similarity total)))
