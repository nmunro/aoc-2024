(defpackage advent-of-code-2024/utils
  (:use :cl)
  (:export #:load-map)
  (:export #:off-grid)
  (:export #:swap-elements))

(in-package advent-of-code-2024/utils)

(define-condition off-grid (error)
    ((message :initarg :message :accessor message)))

(defun load-map (path)
  (let ((data (uiop:read-file-lines path)))
    (make-array (list (length data) (length (first data))) :initial-contents data)))

(defgeneric swap-elements (seq p1 p2)
  (:documentation "Swaps two elements in a sequence, potentially creating a new sequence"))

(defmethod swap-elements ((seq vector) p1 p2)
  (let ((temp (aref seq p1)))
    (setf (aref seq p1) (aref seq p2))
     (setf (aref seq p2) temp))
  seq)

(defmethod swap-elements ((seq list) p1 p2)
  (let ((index 0))
    (mapcar (lambda (item)
              (cond ((= index p1) (prog1 (nth p2 seq) (incf index)))
                    ((= index p2) (prog1 (nth p1 seq) (incf index)))
                    (t (prog1 item (incf index)))))
            seq)))

(defmethod swap-elements ((seq string) p1 p2)
  (let ((new-str (copy-seq seq)))  ; Create a modifiable copy of the string
    (setf (aref new-str p1) (aref seq p2))
    (setf (aref new-str p2) (aref seq p1))
    new-str))
