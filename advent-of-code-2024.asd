(defsystem "advent-of-code-2024"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:cl-arrows
               :cl-utilities)
  :components ((:module "src"
                :components
                ((:file "day1")
                 (:file "main"))))
  :description "Generate a skeleton for modern project"
  :in-order-to ((test-op (test-op "advent-of-code-2024/tests"))))

(defsystem "advent-of-code-2024/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("advent-of-code-2024"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for advent-of-code-2024"
  :perform (test-op (op c) (symbol-call :rove :run c)))