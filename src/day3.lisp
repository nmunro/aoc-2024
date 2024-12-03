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
    (apply #'+ (mapcar #'process-mul muls))))

(defun part-2 (data)
    (loop :with muls = '()
          :with process = t
          :for match :in (ppcre:all-matches-as-strings "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)" data)
          :do (cond ((string-equal match "do()") (setf process t))
                    ((string-equal match "don't()") (setf process nil))
                    (process (push match muls)))
          :finally (return (apply #'+ (mapcar #'part-1 muls)))))

(defun day3 (file)
    (list (part-1 (load-data file)) (part-2 (load-data file))))
