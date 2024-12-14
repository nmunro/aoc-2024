(defpackage advent-of-code-2024/day10
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:load-map #:flatten*)
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
  "Check if the given point is within bounds and its value is start + 1."
  (let ((row (getf point :row))
        (col (getf point :col)))
    (and (>= row 0)
         (>= col 0)
         (< row (array-dimension map 0))  ;; Rows must be within bounds
         (< col (array-dimension map 1))  ;; Columns must be within bounds
         (let ((point-value (digit-char-p (aref map row col)))) ;; Correct indexing
           (and point-value (= (1+ start) point-value))))))

(defun directions (point)
  (let ((north (list :row (1- (getf point :row)) :col (getf point :col)))
        (east  (list :row (getf point :row)      :col (1+ (getf point :col))))
        (south (list :row (1+ (getf point :row)) :col (getf point :col)))
        (west  (list :row (getf point :row) :col (1- (getf point :col)))))
    (list north east south west)))

(defun plot-next-vector (map point)
  (let* ((start (digit-char-p (aref map (getf point :row) (getf point :col)))))
    ;; Return list of VALID vectors
    (remove-if (lambda (point) (not (is-valid-vector-p map point start))) (directions point))))

; @TODO: Write this so that if there's different ways to go
; It can recursively start from the divergent paths
; and join them to the original list at the point
; of divergence
(defun map-trail (map point &key (count 0))
  ; base case
  (if (= 9 count)
    point
    (loop :for next-point :in (plot-next-vector map point)
          :collect (cons point (map-trail map next-point :count (1+ count))))))

(defun part-1 (map)
  (let ((trails (loop :for trailhead :in (find-trailheads map) :collect (map-trail map trailhead))))
    trails))

(defun part-2 (data)
    nil)

(defun day10 (path)
    (let ((map (load-map path)))
      (list (part-1 map) (part-2 map))))

(let ((map (load-map #p"~/quicklisp/local-projects/aoc-2024/data/day10-demo-data.txt")))
  (part-1 map))
