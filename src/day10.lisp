(defpackage advent-of-code-2024/day10
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:load-map)
  (:export #:day10))

(in-package advent-of-code-2024/day10)

(defun find-trailheads (map)
    (apply #'append (loop :for row :from 0 :below (array-dimension map 1)
          :collect (loop :for col :from 0 :below (array-dimension map 0)
                         :if (string-equal (aref map col row) "0")
                         :collect (list :row col :col row)))))

(defun part-1 (data)
    nil)

(defun part-2 (data)
    nil)

(defun day10 (path)
    (let ((map (load-map path)))
      (format t "Map: ~A~%" map)
      (format t "Trailheads: ~A~%" (find-trailheads map))
      (list (part-1 map) (part-2 map))))

(day10 #p"~/quicklisp/local-projects/aoc-2024/data/day10-demo-data.txt")
