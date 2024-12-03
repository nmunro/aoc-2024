(defpackage advent-of-code-2024/day3
  (:use :cl)
  (:export #:day3))

(in-package advent-of-code-2024/day3)

(defun load-data (file)
    (string-trim '(#\Newline) (uiop:read-file-string file)))

(defun process-mul (mul)
    (let ((nums (cl-utilities:split-sequence #\, (string-trim '(#\( #\)) (subseq mul 3)))))
        (apply #'* (mapcar #'parse-integer nums))))

(defun day3 (file)
  (let ((data (load-data file)))
    (let ((muls (ppcre:all-matches-as-strings "mul\\([0-9]{1,3},[0-9]{1,3}\\)" data)))
      (list (apply #'+ (mapcar #'process-mul muls)) nil))))
