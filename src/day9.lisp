(defpackage advent-of-code-2024/day9
  (:use :cl)
  (:import-from :advent-of-code-2024/utils #:swap-elements)
  (:export #:day9))

(in-package advent-of-code-2024/day9)

(defun create-diskmap (map)
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
    (let ((diskmap (coerce (format nil "~{~A~}" (reverse l)) 'list)))
      (make-array (length diskmap) :initial-contents diskmap))))

(defun load-data (data)
  (let ((data (loop :for m :in (coerce (string-trim '(#\Newline) (uiop:read-file-string data)) 'list) :collect (string m))))
    (create-diskmap (mapcar #'parse-integer data))))

(defun generate-file-size-block (num file-id)
  (format nil "~{~A~}" (loop :for x :from 0 :to (1- num) :collect (write-to-string file-id))))

(defun generate-empty-size-block (num)
  (format nil "~{~A~}" (loop :for x :from 0 :to (1- num) :collect ".")))

(defun find-first-free-block (diskmap)
  (position #\. diskmap))

(defun find-last-file-block (diskmap)
  (loop :for i :from (1- (length diskmap)) :downto 0
        :until (not (eq (elt diskmap i) #\.))
        :finally (return i)))

(defun defrag (diskmap)
  (loop :for block :across diskmap
        :for count :from 0 :to (- (length diskmap) 1)
        :when (eql block #\.)
          :do (swap-elements diskmap count (find-last-file-block diskmap)))
  (swap-elements diskmap (find-first-free-block diskmap) (find-last-file-block diskmap)))

(defun calculate-checksum (diskmap)
  (let ((data (loop :for num :across (remove #\. diskmap) :collect (parse-integer (string num)))))
    (loop :for num :in data
          :for count :from 0 :to (1- (length data))
          :sum (* num count))))

(defun part-1 (diskmap)
  (let ((copied-diskmap (copy-seq diskmap)))
    (defrag copied-diskmap)
    (calculate-checksum copied-diskmap)))

(defun part-2 (data)
  nil)

(defun day9 (path)
  (let ((data (load-data path)))
    (list (part-1 data) (part-2 data))))

;; @TODO: This works for ids less than 10, but needs to be extended to support longer file-ids
(let ((data (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day9-demo-data.txt")))
  (= 1928 (part-1 data)))

;; (let ((data (load-data #p"~/quicklisp/local-projects/aoc-2024/data/day9-data.txt")))
;;   (format t "~A~%" data))
  ;; (format t "~A~%" (defrag data)))
