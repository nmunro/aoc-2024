(defpackage advent-of-code-2024/day10
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:load-map)
  (:export #:day10))

(in-package advent-of-code-2024/day10)

(defun find-trailheads (map)
    (apply #'append (loop :for row :from 0 :below (array-dimension map 1)
           :collect (loop :for col :from 0 :below (array-dimension map 0)
                         :if (string-equal (aref map col row) "0")
                         :collect (list :col col :row row)))))

; A vector is valid if it exists within the bounds of the grid
; and the next position is of a higher value than the point
; points need to only be N, S, E, W, only, no diagonals
(defun is-valid-vector-p (map point)
  (and (>= (getf point :row) 0)
       (>= (getf point :col) 0)
       (< (getf point :row) (array-dimension map 1))
       (< (getf point :col) (array-dimension map 0) )))

(defun directions (point)
  (let ((north (list :col (getf point :col) :row (1- (getf point :row))))
        (east (list :col (1+ (getf point :col)) :row (getf point :row)))
        (south (list :col (getf point :col) :row (1+ (getf point :row))))
        (west (list :col (1- (getf point :col)) :row (getf point :row))))
    (list north east south west)))

(defun plot-next-vector (map point)
  (let* ((start (digit-char-p (aref map (getf point :col) (getf point :row))))
         (next (1+ start)))
    ;; Return list of VALID vectors
    (dolist (direction (directions point))
      (format t "Valid: ~A~%" (is-invalid-vector-p map direction)))
    (list start next (remove-if (lambda (point) (not (is-valid-vector-p map point))) (directions point)))))

; @TODO: Write this so that if there's different ways to go
; It can recursively start from the divergent paths
; and join them to the original list at the point
; of divergence
(defun map-trail (map start)
  nil)

(defun part-1 (map)
  (dolist (trailhead (find-trailheads map))
    (format t "Trailhead: ~A, next vectors: ~A~%" trailhead (plot-next-vector map trailhead))))

(defun part-2 (data)
    nil)

(defun day10 (path)
    (let ((map (load-map path)))
      (list (part-1 map) (part-2 map))))

(let ((map (load-map #p"~/quicklisp/local-projects/aoc-2024/data/day10-demo-data.txt")))
  (part-1 map))
