(defpackage advent-of-code-2024/day6
  (:use :cl)
  (:export #:day6))

(in-package advent-of-code-2024/day6)

(defun load-data (file)
  (let ((data (uiop:read-file-lines file)))
    (make-array (list (length data) (length (first data))) :initial-contents data)))

(defun get-pos (map)
  (loop :for y :from 0 :below (array-dimension map 1)
        :do (loop :for x :from 0 :below (array-dimension map 0)
                  :do (when (or (string-equal (aref map x y) "^")
                                (string-equal (aref map x y) ">")
                                (string-equal (aref map x y) "V")
                                (string-equal (aref map x y) "<"))
                        (return-from get-pos (list y x))))))

(defun get-orientation (pos map)
  (aref map (cadr pos) (car pos)))

(defun get-next-pos (current orientation)
  (cond
    ((string= "^" orientation)
     (list (car current) (- (cadr current) 1)))

    ((string= ">" orientation)
     (list (+ (car current) 1) (cadr current)))

    ((string= "V" orientation)
     (list (car current) (+ (cadr current) 1)))

    ((string= "<" orientation)
     (list (- (car current) 1) (cadr current)))

    (t
     nil)))

(defun get-next-orientation (orientation)
  (cond
    ((string= "^" orientation)
     ">")

    ((string= ">" orientation)
     "V")

    ((string= "V" orientation)
     "<")

    ((string= "<" orientation)
     "^")))

(defun collision-p (pos map)
  (string= "#" (aref map (cadr pos) (car pos))))

(defun translate-or-rotate-pos (curr-pos next-pos map)
  (let ((orientation (get-orientation curr-pos map))
        (collision (collision-p next-pos map)))
    (when collision
        (setf (aref map (cadr curr-pos) (car curr-pos)) (get-next-orientation orientation)) ; rotate
        (return-from translate-or-rotate-pos curr-pos))

    (setf (aref map (cadr curr-pos) (car curr-pos)) ".")
    (setf (aref map (cadr next-pos) (car next-pos)) orientation)
    (list (cadr next-pos) (car next-pos)))) ; translate

(defun part-1 (map)
    (let ((coords (make-hash-table :test #'equal)))
      (loop
        (let* ((pos (get-pos map))
               (next-pos (get-next-pos (get-pos map) (get-orientation pos map))))
            (handler-case
                (progn
                  (setf (gethash pos coords) t)
                  (setf (gethash (translate-or-rotate-pos pos next-pos map) coords) t))
              (sb-int:invalid-array-index-error (e)
                (declare (ignore e))
                (return-from part-1 (hash-table-count coords))))))))

(defun part-2 (map)
    nil)

(defun day6 (file)
  (list (part-1 (load-data file)) (part-2 (load-data file))))

(day6 #p"~/quicklisp/local-projects/aoc-2024/data/day6-data.txt")
