(defpackage advent-of-code-2024
  (:use :cl)
  (:import-from :advent-of-code-2024/day1 #:day1)
  (:import-from :advent-of-code-2024/day2 #:day2)
  (:import-from :advent-of-code-2024/day3 #:day3)
  (:import-from :advent-of-code-2024/day4 #:day4)
  (:import-from :advent-of-code-2024/day5 #:day5)
  (:import-from :advent-of-code-2024/day6 #:day6)
  (:export #:main))

(in-package advent-of-code-2024)

(defun main ()
  (format t "Advent Of Code 2024!~%")
  (let ((results (day1 #p"~/quicklisp/local-projects/aoc-2024/data/day1-data.txt")))
    (format t "~tDay 1, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day2 #p"~/quicklisp/local-projects/aoc-2024/data/day2-data.txt")))
    (format t "~tDay 2, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day3 #p"~/quicklisp/local-projects/aoc-2024/data/day3-data.txt")))
    (format t "~tDay 3, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day4 #p"~/quicklisp/local-projects/aoc-2024/data/day4-data.txt")))
    (format t "~tDay 4, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day5 #p"~/quicklisp/local-projects/aoc-2024/data/day5-data.txt")))
    (format t "~tDay 5, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day6 #p"~/quicklisp/local-projects/aoc-2024/data/day6-demo-data.txt")))
    (format t "~tDay 5, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results))))

(main)
