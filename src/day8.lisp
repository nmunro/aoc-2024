(defpackage advent-of-code-2024/day8
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:load-map #:off-grid)
  (:export #:day8))

(in-package advent-of-code-2024/day8)

(defstruct antenna
  freq
  (row 0 :type integer)
  (col 0 :type integer))

(defun flatten* (lst depth &aux (re '()))
  (cond
    ((null lst) '())
    ((listp (car lst))
     (append (cond
               ((= 0 depth)             ; flatten none
                (list (car lst)))
               ((< 0 depth)             ; flatten down
                (flatten* (car lst) (- depth 1)))
               ((= -1 depth)            ; flatten all
                (flatten* (car lst) depth))
               ((< depth -1)            ; flatten up
                (list (flatten* (car lst) (+ depth 1)))))
             (append (flatten* (cdr lst) depth)
                     re)))
    (t (cons (car lst)
             (append (flatten* (cdr lst) depth) re)))))

(defun antenna-diff (a1 a2)
  (let ((row (- (antenna-row a1) (antenna-row a2)))
        (col (- (antenna-col a1) (antenna-col a2))))
    (make-antenna :freq (antenna-freq a1) :row row :col col)))

(defun antenna+ (a1 diff)
  (make-antenna :freq (antenna-freq a1)
                :row (+ (antenna-row a1) (antenna-row diff))
                :col (+ (antenna-col a1) (antenna-col diff))))

(defun antenna- (a1 diff)
  (make-antenna :freq (antenna-freq a1)
                :row (- (antenna-row a1) (antenna-row diff))
                :col (- (antenna-col a1) (antenna-col diff))))

(defun antenna= (a1 a2)
  (and (= (antenna-row a1) (antenna-row a2))
       (= (antenna-col a1) (antenna-col a2))))

(defun get-antinode-pairs (antenna-pairs)
  (list (antenna- (cadr antenna-pairs) (apply #'antenna-diff antenna-pairs))
        (antenna+ (car antenna-pairs) (apply #'antenna-diff antenna-pairs))))

(defun get-antinode-pairs-recur (antenna-pairs map)
  ;; Must be recursive
  (if (and (valid-position-p (car antenna-pairs) map)
           (valid-position-p (cadr antenna-pairs) map))
    (let ((initial-antinodes (get-antinode-pairs antenna-pairs)))
        (list (get-antinode-pairs-recur (list (car antenna-pairs) (car initial-antinodes)) map)
              (get-antinode-pairs-recur (list (cadr antenna-pairs) (cadr initial-antinodes)) map)))
    antenna-pairs))

(defun valid-position-p (antenna map)
    (when (and (>= (antenna-row antenna) 0)
               (>= (antenna-col antenna) 0)
               (< (antenna-row antenna) (array-dimension map 0))
               (< (antenna-col antenna) (array-dimension map 1)))
      antenna))

(defun find-antenna (map)
  (apply #'append (loop :for row :from 0 :below (array-dimension map 1)
                        :collect (loop :for col :from 0 :below (array-dimension map 0)
                                       :if (not (string-equal (aref map col row) "."))
                                       :collect (make-antenna :row col :col row :freq (string (aref map col row)))))))

(defun create-hash (map)
  (let ((hm (make-hash-table :test #'equal)))
        (loop :for antenna :in (find-antenna map)
              :do (setf (gethash (antenna-freq antenna) hm) (push antenna (gethash (antenna-freq antenna) hm))))
        hm))

(defun antenna-pairs (antennas)
  (loop :for i :from 0 :below (length antennas)
        :nconc (loop :for j :from (1+ i) :below (length antennas)
                     :collect (list (nth i antennas) (nth j antennas)))))

(defun get-pairs (hm)
    (loop :for k :being :the :hash-keys :of hm :collect (antenna-pairs (gethash k hm))))

(defun part-1 (map)
  (let ((pairs (apply #'append (get-pairs (create-hash map)))))
    (flet ((valid-pos-p (node) (valid-position-p node map)))
        (length (remove-duplicates
                    (remove nil (mapcar #'valid-pos-p (apply #'append (mapcar #'get-antinode-pairs pairs))))
                    :test #'antenna=)))))

(defun part-2 (map)
  (let ((pairs (apply #'append (get-pairs (create-hash map)))))
    (flet ((valid-pos-p (node) (valid-position-p node map))
           (recur-pairs (pair) (get-antinode-pairs-recur pair map)))
        (length (remove-duplicates (remove nil (mapcar #'valid-pos-p (flatten* (mapcar #'recur-pairs pairs) 1000))))))))

(defun day8 (path)
  (let ((map (load-map path)))
    (list (part-1 map) (part-2 map))))

(= 285 (part-1 (load-map #p"~/quicklisp/local-projects/aoc-2024/data/day8-data.txt")))
(= 34 (part-2 (load-map #p"~/quicklisp/local-projects/aoc-2024/data/day8-demo-data.txt")))

(part-2 (load-map #p"~/quicklisp/local-projects/aoc-2024/data/day8-demo-data.txt"))
