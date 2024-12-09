(defpackage advent-of-code-2024/day7
  (:use :cl)
  (:export #:day7))

(in-package advent-of-code-2024/day7)

(defun load-data (file)
  (uiop:read-file-lines file))

(defun process-permutations (data)
  (loop :for perm :in (permutations '(+ *) (1- (length data)))
        :collect (combine-data-and-permutations data perm)))

(defun process-line (line)
    (let ((data (str:split ": " line)))
        (list :total (parse-integer (car data))
              :data (process-permutations (mapcar #'parse-integer (coerce (str:split " " (cadr data)) 'list))))))

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

(defun sum-totals (totals)
  (apply #'+ totals))

(defun part-1 (data)
    (process-data data))

(defun part-2 (data)
    nil)

(defun day7 (file)
  (list (part-1 (load-data file)) (part-2 (load-data file))))

(part-1 (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day7-demo-data.txt"))
