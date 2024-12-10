(defpackage advent-of-code-2024/day7
  (:use :cl)
  (:export #:day7))

(in-package advent-of-code-2024/day7)

(defun || (a b)
  (parse-integer (format nil "~A~A" a b)))

(defun load-data (file)
  (uiop:read-file-lines file))

(defun evaluate-left-to-right (expr)
  (reduce (lambda (accum next)
            (let ((operator (first next))
                  (operand (second next)))
              (if (numberp accum)
                  (funcall operator accum operand)
                  (funcall (first accum) (second accum) (first next)))))
          (pair-operators-and-operands expr)
          :initial-value (first expr)))

(defun pair-operators-and-operands (expr)
  (loop for i from 1 by 2 to (1- (length expr))
        collect (list (nth i expr) (nth (1+ i) expr))))

(defun process-permutations (data permutations)
  (loop :for perm :in (permutations permutations (1- (length data)))
        :collect (evaluate-left-to-right (combine-data-and-permutations data perm))))

(defun process-line (line permutations)
    (let* ((data (str:split ": " line))
           (total (parse-integer (car data))))
        (member total (process-permutations (mapcar #'parse-integer (coerce (str:split " " (cadr data)) 'list)) permutations))))

(defun process-data (data permutations)
  (remove nil (mapcar (lambda (line) (process-line line permutations)) data)))

(defun permutations (l count)
  (if (= count 1)
      (mapcar #'list l)
      (apply #'append (mapcar (lambda (x) (mapcar (lambda (sub-perm) (cons x sub-perm)) (permutations l (1- count)))) l))))

(defun combine-data-and-permutations (data permutation)
  (remove-duplicates (apply #'append
                            (loop :for x :from 0 :to (length data)
                                  :if (nth x data)
                                  :collect (nth x data) :into res
                                  :if (nth x permutation)
                                  :collect (nth x permutation) :into res
                                  :finally (return-from combine-data-and-permutations res)))))

(defun sum-totals (totals)
  (apply #'+ totals))

(defun part-1 (data)
    (sum-totals (loop :for pd :in (process-data data '(+ *)) :collect (car pd))))

(defun part-2 (data)
    (sum-totals (loop :for pd :in (process-data data '(+ * ||)) :collect (car pd))))

(defun day7 (file)
  (format t "~A~% "(list (part-1 (load-data file)) (part-2 (load-data file)))))

(day7 #p"~/quicklisp/local-projects/aoc-2024/data/day7-data.txt")
