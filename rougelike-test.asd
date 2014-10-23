#|
  This file is a part of rougelike project.
|#

(in-package :cl-user)
(defpackage rougelike-test-asd
  (:use :cl :asdf))
(in-package :rougelike-test-asd)

(defsystem rougelike-test
  :author ""
  :license ""
  :depends-on (:rougelike
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "rougelike"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
