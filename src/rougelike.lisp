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

(defscreen start
           :input ( ((nil) nil)
                    ((#\w) (run-screen (gethash 'win *screens*)))
                    (t (quit-screen)))
           :output ((let ((msg "Welcome to the game.  Press any button.")) 
                     (write-at-point msg 
                                     (floor (/ (- *screen-width* (length msg) ) 2)) 
                                     (floor (/ *screen-height* 2)))))
           :next 'lose
           :boxed t)

(defscreen win
           :input ( ((nil) nil)
                    (t (quit-screen)))
           :output   ((let ((msg "You win")) 
                        (write-at-point msg 
                                     (floor (/ (- *screen-width* (length msg) ) 2)) 
                                     (floor (/ *screen-height* 2))
                                     +green+
                                     )))
           :boxed t)

(defscreen lose
           :input ( ((nil) nil)
                    ((#\q) (quit-screen))
                    ((#\t) (write-at-point  "lose screen input" 3 3)))
           :output ((write-at-point "i am the lose screen" 0 0  +red+))) 


(defun main ()
  (with-init
    (run-screen (gethash 'start *screens*))))

(main)
