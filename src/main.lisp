(defpackage advent-of-code-2024
  (:use :cl)
  (:import-from :advent-of-code-2024/day1 #:day1)
  (:import-from :advent-of-code-2024/day2 #:day2)
  (:import-from :advent-of-code-2024/day3 #:day3)
  (:import-from :advent-of-code-2024/day4 #:day4)
  (:import-from :advent-of-code-2024/day5 #:day5)
  (:import-from :advent-of-code-2024/day6 #:day6)
  (:import-from :advent-of-code-2024/day7 #:day7)
  (:import-from :advent-of-code-2024/day8 #:day8)
  (:import-from :advent-of-code-2024/day9 #:day9)
  (:import-from :advent-of-code-2024/day10 #:day10)
  (:import-from :advent-of-code-2024/day11 #:day11)
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

  (let ((results (day6 #p"~/quicklisp/local-projects/aoc-2024/data/day6-data.txt")))
    (format t "~tDay 6, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day7 #p"~/quicklisp/local-projects/aoc-2024/data/day7-data.txt")))
    (format t "~tDay 7, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day8 #p"~/quicklisp/local-projects/aoc-2024/data/day8-data.txt")))
    (format t "~tDay 8, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day9 #p"~/quicklisp/local-projects/aoc-2024/data/day9-data.txt")))
    (format t "~tDay 9, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day10 #p"~/quicklisp/local-projects/aoc-2024/data/day10-data.txt")))
    (format t "~tDay 10, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results)))

  (let ((results (day11 #p"~/quicklisp/local-projects/aoc-2024/data/day11-data.txt")))
    (format t "~tDay 11, Part 1: ~A, Part 2: ~A~%" (car results) (cadr results))))

(main)
