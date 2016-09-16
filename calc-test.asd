#|
  This file is a part of calc project.
  Copyright (c) 2016 hiroshi kimura (hiroshi.kimura.0331@gmail.com)
|#

(in-package :cl-user)
(defpackage calc-test-asd
  (:use :cl :asdf))
(in-package :calc-test-asd)

(defsystem calc-test
  :author "hiroshi kimura"
  :license ""
  :depends-on (:calc
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "calc"))))
  :description "Test system for calc"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
