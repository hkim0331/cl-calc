#|
  This file is a part of calc project.
  Copyright (c) 2016 hiroshi kimura (hiroshi.kimura.0331@gmail.com)
|#

#|
  Author: hiroshi kimura (hiroshi.kimura.0331@gmail.com)
|#

(in-package :cl-user)
(defpackage calc-asd
  (:use :cl :asdf))
(in-package :calc-asd)

(defsystem calc
  :version "0.1"
  :author "hiroshi kimura"
  :license ""
  :depends-on (:hunchentoot
               :cl-who)
  :components ((:module "src"
                :components
                ((:file "calc"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op calc-test))))
