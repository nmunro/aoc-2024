(defpackage advent-of-code-2024/utils
  (:use :cl)
  (:export #:load-map)
  (:export #:off-grid))

(in-package advent-of-code-2024/utils)

(define-condition off-grid (error)
    ((message :initarg :message :accessor message)))

(defun load-map (path)
  (let ((data (uiop:read-file-lines path)))
    (make-array (list (length data) (length (first data))) :initial-contents data)))
