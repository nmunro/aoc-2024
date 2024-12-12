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
    ((null lst) '())  ;; If list is empty, return empty list
    ((listp (car lst))
     (append (cond
               ((= 0 depth) (list (car lst)))  ;; Don't flatten
               ((< 0 depth) (flatten* (car lst) (- depth 1)))  ;; Flatten down
               ((= -1 depth) (flatten* (car lst) depth))  ;; Flatten all
               ((< depth -1) (list (flatten* (car lst) (+ depth 1)))))  ;; Flatten up
             (flatten* (cdr lst) depth)))
    (t (cons (car lst) (flatten* (cdr lst) depth)))))

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

(defun map-pair-to-antinodes (pair map)
  (flet ((plot-next-antinodes (antenna diff map)
            (if (not (valid-position-p antenna map))
                nil
                (cons antenna (plot-next-antinodes (antenna+ antenna diff) diff map)))))
    (let ((d1 (antenna- (car pair) (cadr pair)))
          (d2 (antenna- (cadr pair) (car pair))))
      (remove-if (lambda (antinode) (member antinode pair :test #'antenna=))
                 (apply #'append (list
                                  (plot-next-antinodes (car pair) d1 map)
                                  (plot-next-antinodes (cadr pair) d2 map)))))))

(defun part-1 (map)
  (let ((pairs (apply #'append (get-pairs (create-hash map)))))
    (flet ((valid-pos-p (node) (valid-position-p node map)))
        (length (remove-duplicates
                    (remove nil (mapcar #'valid-pos-p (apply #'append (mapcar #'get-antinode-pairs pairs))))
                    :test #'antenna=)))))

;; @TODO: Need to add antenna that are NOT already in the list of antinodes
(defun part-2 (map)
  (flet ((map-pair (pair) (map-pair-to-antinodes pair map)))
    (let* ((antinodes (remove-duplicates (flatten* (mapcar #'map-pair (apply #'append (get-pairs (create-hash map)))) 1000) :test #'antenna=))
           (left-over-antenna (loop :for antenna :in (find-antenna map) :unless (member antenna antinodes :test #'antenna=) :collect antenna)))
        (+ (length left-over-antenna) (length antinodes)))))

(defun day8 (path)
  (let ((map (load-map path)))
    (list (part-1 map) (part-2 map))))
