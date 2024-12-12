(defpackage advent-of-code-2024/day6
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:load-map #:off-grid)
  (:export #:day6))

(in-package advent-of-code-2024/day6)

(define-condition walked-too-far (error)
    ((message :initarg :message :accessor message)))

(defun get-pos (map)
  (loop :for y :from 0 :below (array-dimension map 1)
        :do (loop :for x :from 0 :below (array-dimension map 0)
                  :when (member (aref map x y) '(#\^ #\> #\V #\<))
                  :do (return-from get-pos (list y x))))
  (error 'off-grid :message "Walked off the grid"))

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
  (let ((orientation (get-orientation curr-pos map)))
    (if (collision-p next-pos map)
        (progn
          (setf (aref map (cadr curr-pos) (car curr-pos)) (get-next-orientation orientation)) ; Rotate
          curr-pos)
      (progn
        (setf (aref map (cadr curr-pos) (car curr-pos)) ".")
        (setf (aref map (cadr next-pos) (car next-pos)) orientation)
        next-pos)))) ; Translate

(defun next-obstacle-location (map)
  "Returns a generator that finds the next '.' in a 2D array, resuming from the last search point."
  (let ((point '(0 0))) ; Starting point for the search
    (lambda ()
      (block generator
        (loop :for y :from (second point) :below (array-dimension map 0)
              :do (loop :for x :from (first point) :below (array-dimension map 1)
                        :when (string= (aref map y x) ".")
                        :do (progn
                              (setf point (list (1+ x) y))
                              (return-from generator (list y x))))
              (setf point (list 0 (1+ y))))
        nil))))

(defun generate-new-map (map point)
  (let ((new-map (cl-utilities:copy-array map)))
    (setf (aref new-map (car point) (cadr point)) #\#)
    new-map))

(defun traverse-map (map step-limit)
  (let ((visited (make-hash-table :test #'equal))
        (pos (get-pos map))
        (steps-taken 0))
    (loop
      (handler-case
          (progn
            (when (= step-limit steps-taken)
              (error 'walked-too-far :message "Walked in a loop"))

            (setf (gethash pos visited) t) ; Mark current position as visited
            (let* ((orientation (get-orientation pos map))
                   (next-pos (get-next-pos pos orientation)))
              (setf pos (translate-or-rotate-pos pos next-pos map))
              (incf steps-taken))) ; Move to the next position

        (walked-too-far (e)
          (declare (ignore e))
          (return-from traverse-map (values 1 (hash-table-count visited))))

        (off-grid (e) ; Exit when the character goes off the grid
          (declare (ignore e))
          (return-from traverse-map (values 0 (hash-table-count visited))))

        (sb-int:invalid-array-index-error (e) ; Handle out-of-bounds errors
          (declare (ignore e))
          (return-from traverse-map (values 0 (hash-table-count visited))))))))

(defun part-1 (map)
  "Counts the number of unique nodes visited in the original map."
  (multiple-value-bind (loops unique-nodes)
      (traverse-map map 10000)
    (declare (ignore loops))
    unique-nodes))

(defun part-2 (map)
  (let ((fn (next-obstacle-location map))
        (loops-count 0))
    (loop
      (let ((result (funcall fn)))
        (if result
            (multiple-value-bind (loops unique-nodes)
                (traverse-map (generate-new-map map result) 10000)
              (declare (ignore unique-nodes))
              (when (= 1 loops)
                (incf loops-count)))
            (return-from part-2 loops-count))))))

(defun day6 (file)
  (list (part-1 (load-map file)) (part-2 (load-map file))))
