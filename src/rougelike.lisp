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

(defun center (w h)
  (values
   (floor (/ w 2))
   (floor (/ h 2))))

(defun write-at-center (msg &optional (color +white+))
  (write-at-point msg
   (floor (/ (- *screen-width* (length msg)) 2))
   (floor (/ *screen-height* 2))
   color
   ))

(defparameter *player* nil)
(defstruct player x y health)

(defun init-player ()
  (setf *player* (make-player 
                   :x (random *screen-width*) 
                   :y (random *screen-height*) )))

(defun draw-player (p)
  (write-at-point 
    #\@
    (player-x p)
    (player-x p)
    +magenta+))


(defparameter *grid* nil)
(defparameter *world-width*  1000)
(defparameter *world-height*  1000)

(defun draw-map ()
(setf *grid* (perlin2d-grid   *world-width* *world-height*  0.1 4) )  )


  
(defscreen start
           :input ( ((nil) nil)
                    (t (quit-screen)))
           :output ((write-at-center "Welcome to the game.  Press any button."))
           :next 'play
           :boxed t)


(defscreen play
           :before ((init-player))
           :input ( ((nil) nil)
                    ((#\w) (run-screen (gethash 'win *screens*)))
                    (t (quit-screen)))
           :output ((draw-player *player*))
           :next 'lose
           :boxed t)

(defscreen win
           :input ( ((nil) nil)
                    (t (quit-screen)))
           :output ((write-at-center "You win" +green+))
           :boxed t)

(defscreen lose
           :input ( ((nil) nil)
                    (t (quit-screen)))
           :output ((write-at-center "You lose" +red+))
           :boxed t)



(defun main ()
  (with-init
    (run-screen (gethash 'start *screens*))))

(main)
