(defpackage advent-of-code-2024/day11
  (:use :cl)
  (:export #:day11))

(in-package advent-of-code-2024/day11)

(defun load-data (path)
  (mapcar #'parse-integer (str:split " " (uiop:read-file-line path))))

(defun even-digit-p (number)
  (evenp (floor (1+ (log number 10)))))

(defun process-stone (stone)
  (flet ((split-stone (stone)
           (let* ((stone-num (write-to-string stone))
                  (split-point (/ (length stone-num) 2)))
             (list (parse-integer (subseq stone-num 0 split-point)) (parse-integer (subseq stone-num split-point))))))
    (cond
      ((= 0 stone)
       1)

      ((even-digit-p stone)
       (split-stone stone))

      (t
       (* stone 2024)))))

(defun process-stones (stones)
  (loop :for stone :in stones
        :for processed-stone = (process-stone stone)
        :if (listp processed-stone)
          :collect (car processed-stone)
          :and
          :collect (cadr processed-stone)
        :else
          :collect processed-stone))

(defun blink (stones &key (count 0) (max 0))
  (if (= count max)
      stones
      (blink (process-stones stones) :count (1+ count) :max max)))

(defun part-1 (stones)
  (let ((stones (copy-list stones)))
    (length (blink stones :max 25))))

(defun part-2 (stones)
  (let ((stones (copy-list stones)))
    (length (blink stones :max 25))))

(defun day11 (path)
    (let ((stones (load-data path)))
      (list (part-1 stones) (part-2 stones))))
