#|
  This file is a part of rougelike project.
|#

(in-package :cl-user)
(defpackage rougelike-asd
  (:use :cl :asdf))
(in-package :rougelike-asd)

(defsystem rougelike
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cl-charms
               :perlin
               :omens)
  :components ((:module "src"
                :components
                ((:file "rougelike"))))
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
  :in-order-to ((test-op (test-op rougelike-test))))
