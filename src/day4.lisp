(defpackage advent-of-code-2024/day4
  (:use :cl)
  (:export #:day4))

(in-package advent-of-code-2024/day4)

(defun load-data (file)
  (let ((lines (mapcar (lambda (line) (coerce line 'list)) (uiop:read-file-lines file))))
    (make-array (list (length lines) (length (first lines))) :initial-contents lines)))

(defun part-1-coordinates (origin)
  (let ((x (car origin))
        (y (cadr origin)))
    (list
     ;; North (Up)
     (list (list x y) (list x (- y 1)) (list x (- y 2)) (list x (- y 3)))
     ;; North-East (Up-Right)
     (list (list x y) (list (+ x 1) (- y 1)) (list (+ x 2) (- y 2)) (list (+ x 3) (- y 3)))
     ;; East (Right)
     (list (list x y) (list (+ x 1) y) (list (+ x 2) y) (list (+ x 3) y))
     ;; South-East (Down-Right)
     (list (list x y) (list (+ x 1) (+ y 1)) (list (+ x 2) (+ y 2)) (list (+ x 3) (+ y 3)))
     ;; South (Down)
     (list (list x y) (list x (+ y 1)) (list x (+ y 2)) (list x (+ y 3)))
     ;; South-West (Down-Left)
     (list (list x y) (list (- x 1) (+ y 1)) (list (- x 2) (+ y 2)) (list (- x 3) (+ y 3)))
     ;; West (Left)
     (list (list x y) (list (- x 1) y) (list (- x 2) y) (list (- x 3) y))
     ;; North-West (Up-Left)
     (list (list x y) (list (- x 1) (- y 1)) (list (- x 2) (- y 2)) (list (- x 3) (- y 3))))))

(defun part-2-coordinates (origin)
  (let ((x (car origin))
        (y (cadr origin)))
    (list (list (list (- x 1) (- y 1)) (list (+ x 1) (- y 1)) (list x y) (list (- x 1) (+ y 1)) (list (+ x 1) (+ y 1))))))

(defun invalid-coord-p (coord grid)
  (or (< (car coord) 0)
      (< (cadr coord) 0)
      (>= (car coord) (array-dimension grid 1))
      (>= (cadr coord) (array-dimension grid 0))))

(defun filter-invalid-coordinates (coord-sets grid)
  (remove-if (lambda (coord-set) (some (lambda (coord) (invalid-coord-p coord grid)) coord-set)) coord-sets))

(defun find-each (char grid)
  (apply #'append (loop :for x :below (array-dimension grid 1)
                               :collect (loop :for y :below (array-dimension grid 0)
                                              :when (eq char (aref grid x y))
                                              :collect `(,x ,y)))))

(defun get-valid-coordinates-for-point (point grid fn)
    (filter-invalid-coordinates (funcall fn point) grid))

(defun match-1-p (coords grid)
  (equal (loop :for coord :in coords :collect (aref grid (car coord) (cadr coord))) '(#\X #\M #\A #\S)))

(defun match-2-p (coords grid)
  (let ((d1 (loop :for d :in (list (nth 0 coords) (nth 2 coords) (nth 4 coords)) :collect (aref grid (nth 0 d) (nth 1 d))))
        (d2 (loop :for d :in (list (nth 1 coords) (nth 2 coords) (nth 3 coords)) :collect (aref grid (nth 0 d) (nth 1 d)))))
    (or (and (equal d1 '(#\M #\A #\S)) (equal d2 '(#\M #\A #\S)))
        (and (equal d1 '(#\M #\A #\S)) (equal d2 '(#\S #\A #\M)))
        (and (equal d1 '(#\S #\A #\M)) (equal d2 '(#\M #\A #\S)))
        (and (equal d1 '(#\S #\A #\M)) (equal d2 '(#\S #\A #\M))))))

(defun count-totals (items)
  (length (remove-if #'null (apply #'append items))))

(defun process (grid char fn match)
  (loop :for point :in (find-each char grid)
        :collect (loop :for coords :in (get-valid-coordinates-for-point point grid fn)
                       :when (funcall match coords grid)
                       :collect 1)))

(defun day4 (file)
  (let ((grid (load-data file)))
    (list (count-totals (process grid #\X #'part-1-coordinates #'match-1-p))
          (count-totals (process grid #\A #'part-2-coordinates #'match-2-p)))))
