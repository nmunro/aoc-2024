(defpackage advent-of-code-2024/day11
  (:use :cl)
  (:export #:day11))

(in-package advent-of-code-2024/day11)

(defun load-data (path)
  (mapcar #'parse-integer (str:split " " (uiop:read-file-line path))))

(defun even-digit-p (number)
  (evenp (floor (1+ (log number 10)))))

(defun split-stone (stone)
  (let* ((stone-num (write-to-string stone))
         (split-point (/ (length stone-num) 2)))
    (list (parse-integer (subseq stone-num 0 split-point)) (parse-integer (subseq stone-num split-point)))))

(fare-memoization:define-memo-function process-stone (stone)
    (cond
      ((= 0 stone)
       (list 1))

      ((even-digit-p stone)
       (split-stone stone))

      (t
       (list (* stone 2024)))))

(fare-memoization:define-memo-function blink (stone max)
  (if (zerop max)
      1
      (reduce #'+ (mapcar (lambda (new-stone) (blink new-stone (1- max))) (process-stone stone)))))

(defun part-1 (stones)
  (loop :for stone :in stones :summing (blink stone 25)))

(defun part-2 (stones)
  (loop :for stone :in stones :summing (blink stone 75)))

(defun day11 (path)
  (let ((stones (load-data path)))
    (list (part-1 stones) (part-2 stones))))
