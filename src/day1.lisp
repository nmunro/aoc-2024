(defpackage advent-of-code-2024/day1
  (:use :cl)
  (:export #:day1))

(in-package advent-of-code-2024/day1)

(defun load-data (file)
    (with-open-file (data file :direction :input :if-does-not-exist :error)
      (flet ((split-line (line) (remove-if (lambda (str) (string-equal str "")) (cl-utilities:split-sequence #\Space line))))
        (loop :for raw-line = (read-line data nil)
              :while raw-line
              :for line = (split-line raw-line)
                    :collect (parse-integer (car line)) :into list1
                    :collect (parse-integer (cadr line)) :into list2
              :finally (return (list list1 list2))))))

(defun sort-data (lists)
    (list (sort (car lists) '<)
          (sort (cadr lists) '<)))

(defun total (data)
    (apply #'+ data))

(defun get-difference (lists)
    (loop :for num1 :in (car lists)
          :for num2 :in (cadr lists)
          :collect (abs (- num2 num1))))

(defun get-similarity (lists)
    (loop :for num :in (car lists)
          :collect (* num (count-if (lambda (x) (= x num)) (cadr lists)))))

(defun day1 (file)
    (list (arrows:-> (load-data file) sort-data get-difference total)
          (arrows:-> (load-data file) sort-data get-similarity total)))
