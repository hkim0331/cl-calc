(in-package :cl-user)
(defpackage calc-test
  (:use :cl
        :calc
        :prove))
(in-package :calc-test)

;; NOTE: To run this test file, execute `(asdf:test-system :calc)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
