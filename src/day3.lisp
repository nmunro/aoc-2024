(defpackage advent-of-code-2024/day3
  (:use :cl)
  (:export #:day3))

(in-package advent-of-code-2024/day3)

(defun load-data (file)
    (string-trim '(#\Newline) (uiop:read-file-string file)))

(defun process-mul (mul)
    (let ((nums (cl-utilities:split-sequence #\, (string-trim '(#\( #\)) (subseq mul 3)))))
        (apply #'* (mapcar #'parse-integer nums))))

(defun part-1 (data)
  (let ((muls (ppcre:all-matches-as-strings "mul\\([0-9]{1,3},[0-9]{1,3}\\)" data)))
    (apply #'+ (mapcar #'process-mul-1 muls))))

(defun part-2 (data)
    (let* ((matches (ppcre:all-matches-as-strings "(?<=do\\(\\))(.*?)(?=don't\\(\\)|$)" data)))
        (apply #'+ (loop :for match :in matches :collect (part-1 match)))))

(defun day3 (file)
    (list (part-1 (load-data file)) (part-2 (load-data file))))

(let* ((data (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day3-data.txt"))
       (matches (ppcre:all-matches-as-strings "(?<=do\\(\\))(.*?)(?=don't\\(\\)|$)")))
  (format t "~A~%" matches))
