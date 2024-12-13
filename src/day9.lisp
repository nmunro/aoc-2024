(defpackage advent-of-code-2024/day9
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:swap-elements #:flatten*)
  (:export #:day9))

(in-package advent-of-code-2024/day9)

(defun create-diskmap (map)
  (flet ((generate-file-size-block (num file-id) (loop :for x :from 0 :to (1- num) :collect file-id))
         (generate-empty-size-block (num) (loop :for x :from 0 :to (1- num) :collect ".")))
    (let ((file-id 0)
          (l '()))
      (loop :for block :in map
            :for x :from 0 :to (length map)
            :if (evenp x)
              :do (progn
                    (push (generate-file-size-block block file-id) l)
                    (incf file-id))
            :else
              :do
                  (push (generate-empty-size-block block) l))
      (let ((diskmap (flatten* (remove nil (reverse l)) 1000)))
        (make-array (length diskmap) :initial-contents diskmap)))))

(defun load-data (data)
  (let ((data (loop :for m :in (coerce (string-trim '(#\Newline) (uiop:read-file-string data)) 'list) :collect (string m))))
    (create-diskmap (mapcar #'parse-integer data))))

(defun find-last-file-block (diskmap)
  (loop :for idx :from (1- (length diskmap)) :downto 0
        :for char = (elt diskmap idx)
        :until (not (and (stringp char) (string= char ".")))
        :finally (return idx)))

(defun defrag-1 (diskmap)
  (flet ((is-sorted-p (diskmap index)
           (loop :for i :from index :to (1- (length diskmap))
                 :always (and (stringp (elt diskmap i)) (string= (elt diskmap i) ".")))))
    (dotimes (x (length diskmap))
      (when (is-sorted-p diskmap x)
        (return diskmap))
      (let ((char (elt diskmap x)))
        (when (and (stringp char) (string= char "."))
          (swap-elements diskmap x (find-last-file-block diskmap)))))
    diskmap))

(defun swap-file-blocks (diskmap file-block)
  (loop :with free-space-blocks := (find-x-free-blocks diskmap (size file-block))
        :for index :from (file-block-start file-block) :to (file-block-end file-block)
        :for free-space-block := (first free-space-blocks)
        :while (and free-space-block (< free-space-block index))  ; Only move left
        :do (progn
              (swap-elements diskmap index free-space-block)
              (setf free-space-blocks (rest free-space-blocks)))))

(defun defrag-2 (diskmap)
  ; Store files that have been attemped to defragged, successfully or not
  (let ((defragged-files '()))
    (loop :for file-block :across (map-file-blocks diskmap)
          :do (swap-file-blocks diskmap file-block))
    diskmap))

(defun calculate-checksum (diskmap)
  (loop :for num :across diskmap
        :for count :from 0 :to (1- (length diskmap))
        :if (numberp num)
        :sum (* num count)))

(defstruct file-block
  file-id
  start
  end)

(defmethod size ((file-block file-block))
  (1+ (- (file-block-end file-block) (file-block-start file-block))))

(defun blocks-to-file-block (blocks)
  (let ((first-block (car blocks)))
    (make-file-block :file-id (getf first-block :file-id)
                     :start (getf first-block :start)
                     :end (+ (1- (length blocks)) (getf first-block :start)))))

(defun map-file-blocks (diskmap)
  (let ((res '())
        (current-block '())
        (file-id nil))
    (loop :for block :across diskmap
          :for index :from 0
          :do (cond
                ;; Encountering a new file ID, flush current block
                ((and (numberp block)
                      (or (null current-block)  ; No current block
                          (/= file-id block))) ; Different file ID
                 ;; Flush the current block if it exists
                 (when current-block
                   (push (blocks-to-file-block (reverse current-block)) res))
                 ;; Start a new block for the new file ID
                 (setf current-block (list (list :start index :file-id block))
                       file-id block))

                ;; Continuing the same file ID
                ((and (numberp block) (= file-id block))
                 (push (list :start index :file-id file-id) current-block))

                ;; Encountering a ".", flush the current block
                ((and (stringp block) (string= block "."))
                 (when current-block
                   (push (blocks-to-file-block (reverse current-block)) res))
                 ;; Reset current block and file-id
                 (setf current-block '() file-id nil)))
          ;; Finally, handle the last block if any
          :finally (when current-block
                     (push (blocks-to-file-block (reverse current-block)) res)))
    ;; Return results as a vector
    (coerce res 'vector)))

(defun find-x-free-blocks (diskmap num-of-blocks)
  (let ((current-count 0)
        (start-index nil))
    (loop :for elem :across diskmap
          :for index :from 0
          :do (cond
                ((and (stringp elem) (string= elem "."))
                 (when (null start-index)
                   (setf start-index index))
                 (incf current-count)
                 (when (= current-count num-of-blocks)
                  (return (loop :for x :from start-index :to (+ start-index num-of-blocks -1) :collect x))))

                (t
                 (progn
                  (setf current-count 0)
                  (setf start-index nil)))))))

(defun part-1 (diskmap)
  (let ((copied-diskmap (copy-seq diskmap)))
    (defrag-1 copied-diskmap)
    (calculate-checksum copied-diskmap)))

(defun part-2 (diskmap)
  (let ((copied-diskmap (copy-seq diskmap)))
    (defrag-2 copied-diskmap)
    (calculate-checksum copied-diskmap)))

(defun day9 (path)
  (let ((data (load-data path)))
    (list (part-1 data) (part-2 data))))
