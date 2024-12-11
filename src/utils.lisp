(defpackage advent-of-code-2024/utils
  (:use :cl)
  (:export #:update-progress-bar))

(in-package advent-of-code-2024/utils)

(defun load-map (path)
  (let ((data (uiop:read-file-lines path)))
    (make-array (list (length data) (length (first data))) :initial-contents data)))
