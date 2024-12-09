(defpackage advent-of-code-2024/day7
  (:use :cl)
  (:export #:day7))

(in-package advent-of-code-2024/day7)

(defun load-data (file)
  (uiop:read-file-lines file))

(defun process-line (line)
    (let ((data (str:split ": " line)))
        (list :total (parse-integer (car data)) :data (mapcar #'parse-integer (coerce (str:split " " (cadr data)) 'list)))))

(defun process-data (data)
  (mapcar (lambda (line) (process-line line)) data))

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

(defun eval-calculation (calc)
  (eval (cons 'mexpr:infix calc)))

(defun check-eval-calculation (total calc)
  (format t "Total: ~A, Calc: ~A~%" total calc)
  (= total (eval-calculation calc)))

(defun sum-totals (totals)
  (apply #'+ totals))

(defun process (data)
  (loop :for permutation :in (permutations '(+ *) (- (length data) 1))
        :collect (combine-data-and-permutations data permutation)))

(defun part-1 (data)
    (let ((processed-data (process-data data)))
      (loop :for data :in processed-data
            ;; :if (check-eval-calculation (getf data :total) (process (getf data :data)))
            :collect (process (getf data :data)))))

(defun part-2 (data)
    nil)

(defun day7 (file)
  (list (part-1 (load-data file)) (part-2 (load-data file))))

(check-eval-calculation 43 '(10 + 19 + 14))
(check-eval-calculation 43 '(10 + 19))

(part-1 (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day7-demo-data.txt"))
