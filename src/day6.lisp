(defpackage advent-of-code-2024/day6
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:load-map)
  (:export #:day6))

(in-package advent-of-code-2024/day6)

(define-condition off-grid (error)
    ((message :initarg :message :accessor message)))

(defun load-data (file)
  (let ((data (uiop:read-file-lines file)))
    (make-array (list (length data) (length (first data))) :initial-contents data)))

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

(defun blank-spaces (map)
  (let ((spaces '()))
    (loop :for y :from 0 :below (array-dimension map 1)
          :do (loop :for x :from 0 :below (array-dimension map 0)
                    :when (string= (aref map y x) ".")
                    :do (push (list y x) spaces)))
    spaces))

(defun altered-maps (map)
  "Returns a generator that modifies each blank space in the map and reverts the change after processing."
  (let ((spaces (blank-spaces map)))
    (lambda ()
      (when spaces
        (let* ((point (pop spaces))
               (copy (make-array (array-dimensions map)
                                 :initial-contents (map 'list #'copy-seq map))))
          ;; Modify the map copy at the blank space
          (setf (aref copy (car point) (cadr point)) "#")
          copy))))) ; Return the modified map

(defun altered-maps (map)
  "Returns a generator that modifies each blank space in the map and reverts the change after processing."
  (let ((spaces (blank-spaces map)))
    (lambda ()
      (when spaces
        (let* ((point (pop spaces))
               (copy (cl-utilities:copy-array map))          ; Make a copy of the map
               (revert-fn (alter-map copy point "#"))) ; Modify the copy
          (funcall revert-fn) ; Restore the original map after use
          copy))))) ; Return the modified copy

(defun part-1 (map)
  "Counts the number of unique nodes visited in the original map."
  (let ((visited (make-hash-table :test #'equal))
        (pos (get-pos map)))
    (loop
      (handler-case
          (progn
            (setf (gethash pos visited) t) ; Mark current position as visited
            (let* ((orientation (get-orientation pos map))
                   (next-pos (get-next-pos pos orientation)))
              (setf pos (translate-or-rotate-pos pos next-pos map)))) ; Move to the next position

        (off-grid (e) ; Exit when the character goes off the grid
          (declare (ignore e))
          (return-from part-1 (hash-table-count visited)))

        (sb-int:invalid-array-index-error (e) ; Handle out-of-bounds errors
          (declare (ignore e))
          (return-from part-1 (hash-table-count visited)))))))

(defun check-loop (map)
  "Returns T if the character takes more than 10,000 steps without going off-grid; NIL otherwise."
  (let ((step-count 0)
        (pos (get-pos map)))
    (loop
      (handler-case
          (progn
            (let* ((orientation (get-orientation pos map))
                   (next-pos (get-next-pos pos orientation)))
              (setf pos (translate-or-rotate-pos pos next-pos map)))
            (incf step-count)
            (when (> step-count 10000)
              (return-from check-loop t))) ; Return T if step count exceeds 10,000

        (off-grid (e) ; Exit if the character goes off the grid
          (declare (ignore e))
          (return-from check-loop nil))

        (sb-int:invalid-array-index-error (e) ; Handle out-of-bounds errors
          (declare (ignore e))
          (return-from check-loop nil))))))

(defun part-2 (map)
  "Counts how many altered maps cause the character to take more than 10,000 steps (loop)."
  (let ((generator (altered-maps map))
        (loop-count 0)
        (map-count 0)
        (map-total (length (blank-spaces map))))
    (loop
      (let ((altered-map (funcall generator)))
        (when altered-map
          (incf map-count)
          (format t "Processing map ~A/~A~%" map-count map-total)
          (when (check-loop altered-map)
            (incf loop-count)))
        (unless altered-map
          (return loop-count))))))

(defun day6 (file)
  "Processes the input file for both parts."
  (let ((map (load-map file)))
    (list (part-1 map) (part-2 map))))

(defun day6 (file)
  (list (part-1 (load-map file)) (part-2 (load-map file))))
