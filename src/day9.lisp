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

(defun defrag-2 (diskmap)
  nil)

(defun calculate-checksum-1 (diskmap)
  (let ((data (subseq diskmap 0 (position "." diskmap :test #'equal))))
    (loop :for num :across data
          :for count :from 0 :to (1- (length data))
          :sum (* num count))))

(defun calculate-checksum-2 (diskmap)
  nil)

(defun part-1 (diskmap)
  (let ((copied-diskmap (copy-seq diskmap)))
    (defrag-1 copied-diskmap)
    (calculate-checksum-1 copied-diskmap)))

(defun part-2 (diskmap)
  (let ((copied-diskmap (copy-seq diskmap)))
    copied-diskmap))

(defun day9 (path)
  (let ((data (load-data path)))
    (list (part-1 data) (part-2 data))))

(let ((data (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day9-demo-data.txt")))
  (part-1 data))
