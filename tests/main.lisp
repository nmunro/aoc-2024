(defpackage advent-of-code-2024/tests/main
  (:use :cl
        :advent-of-code-2024
        :rove))
(in-package :advent-of-code-2024/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :advent-of-code-2024)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))