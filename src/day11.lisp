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

(defun process-stones-in-place (stones memo)
  "Processes stones in place, updating the list without creating a new one."
  (let ((new-stones (list)))
    (dolist (stone stones)
      (let ((processed-stone (funcall memo stone)))
        (if (listp processed-stone)
            (setf new-stones (nconc new-stones processed-stone))
            (push processed-stone new-stones))))
    (nreverse new-stones))) ;; Return the modified list

(defun blink (stones max memo)
  (loop :with current-stones := stones
        :for x :from 0 :to (1- max)
        :do (setf current-stones (process-stones-in-place current-stones memo))
        :finally (return current-stones)))

(defun part-1 (stones memo)
  (let ((stones (copy-list stones)))
    (length (blink stones 25 memo))))

(defun part-2 (stones memo)
  (let ((stones (copy-list stones)))
    (length (blink stones 75 memo))))

(defun day11 (path)
  (let* ((stones (load-data path))
         (db (sqlite:connect ":memory:"))
         (memo (memoize #'process-stone db)))
    (unwind-protect
         (list (part-1 stones memo)
               (part-2 stones memo))
      (sqlite:disconnect db))))

;; (let* ((db (sqlite:connect ":memory:"))
;;        (memo (memoize #'process-stone db)))
;;   (let ((res (funcall memo 99)))
;;     (format t "~A~%" (sqlite:execute-to-list db "SELECT * FROM cache"))
;;     (sqlite:disconnect db)
;;     res))
