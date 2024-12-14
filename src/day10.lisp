(defpackage advent-of-code-2024/day10
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:load-map)
  (:export #:day10))

(in-package advent-of-code-2024/day10)

(defun find-trailheads (map)
    (apply #'append (loop :for x :from 0 :below (array-dimension map 1)
           :collect (loop :for y :from 0 :below (array-dimension map 0)
                         :if (string-equal (aref map y x) "0")
                         :collect (list :row y :col x)))))

; A vector is valid if it exists within the bounds of the grid
; and the next position is of a higher value than the point
; points need to only be N, S, E, W, only, no diagonals
(defun is-valid-vector-p (map point start)
  (handler-case
    (let ((point-value (digit-char-p (aref map (getf point :col) (getf point :row)))))
      (and (>= (getf point :row) 0)
           (>= (getf point :col) 0)
           (< (getf point :row) (array-dimension map 1))
           (< (getf point :col) (array-dimension map 0))
           (= (1+ start) point-value)))
    (sb-int:invalid-array-index-error (e)
      (declare (ignore e))
      (return-from is-valid-vector-p nil))))

(defun directions (point)
  (let ((north (list :col (1- (getf point :row)) :row (getf point :col)))
        (east (list :col (getf point :row) :row (1+ (getf point :col))))
        (south (list :col (1+ (getf point :row)) :row (getf point :col)))
        (west (list :col (getf point :row) :row (1- (getf point :col)))))
    (list north east south west)))

(defun plot-next-vector (map point)
  (let* ((start (digit-char-p (aref map (getf point :row) (getf point :col)))))
    ;; Return list of VALID vectors
    (list start start (remove-if (lambda (point) (not (is-valid-vector-p map point start))) (directions point)))))

; @TODO: Write this so that if there's different ways to go
; It can recursively start from the divergent paths
; and join them to the original list at the point
; of divergence
(defun map-trail (map start)
  (format t "Start: ~A, next vectors: ~A~%" start (plot-next-vector map start)))

(defun part-1 (map)
  (dolist (trailhead (find-trailheads map))
    (map-trail trailhead)))

(defun part-2 (data)
    nil)

(defun day10 (path)
    (let ((map (load-map path)))
      (list (part-1 map) (part-2 map))))

(let ((map (load-map #p"~/quicklisp/local-projects/aoc-2024/data/day10-demo-data.txt")))
  (part-1 map))
