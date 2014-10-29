(in-package :cl-user)

(ql:quickload "cl-charms")
(ql:quickload "perlin")
(ql:quickload "omens")
(defpackage rougelike
  (:use :cl
        :cl-charms
        :perlin
        :omens))
(in-package :rougelike)

(defscreen win
           :input ( ((nil) nil)
                    ((#\q) (setf *running* nil))
                    ((#\t) (write-at-point "win screen input" 3 3 4)))
           :output ((write-at-point "i am the win screen" 0 0))
           :next 'lose)

(defscreen lose
           :input ( ((nil) nil)
                    ((#\q) (setf *running* nil))
                    ((#\t) (write-at-point  "lose screen input" 3 3)))
           :output ((write-at-point "i am the lose screen" 0 0  +red+)
                    )) 


(defun main ()
  (with-init
    (run-screen (gethash 'win *screens*))))

(main)
