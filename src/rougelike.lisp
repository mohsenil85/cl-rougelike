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

(defun write-at-center (msg &optional (color +white+))
  (write-at-point msg
                  (floor (/ (- *screen-width* (length msg)) 2))
                  (floor (/ *screen-height* 2))
                  color))

(defparameter *player* nil)
(defstruct player x y health)
(defstruct tile x y glyph color)

(defun init-player ()
  (setf *player* (make-player 
                   :x 45 ;(random 900) ; 1043;45 ;(random *world-width*)
                   :y -45 ; 953; ; (random *world-height*)
                   )))

(setf *seed* (random 10))

(defun calc-origin-from-player-coords (x y)
  (values 
    (- (- y (floor (/ *screen-height*  2))) 27)
    (- (- x (floor (/  *screen-width*  2))) 32 )
    ))

(defun draw-hud ()
  (write-at-point 
    (format nil "X: ~A Y: ~A" 
            (player-x *player*)
            (player-y *player*)
            )
   ;         (- (player-x *player*) 45) 
   ;         (+ (player-y *player*) 45))
    1 1)
  )

(defun perlin-lookup (x y)
  (if (and
        (> y 0 )  
        (> x 0 )  
        (< y *world-height* )  
        (< x *world-width* ))
    (aref *grid* y x)
    0.0
    ))


(defun draw-map (player) 
  (let ((abs-x 
          (+ 1 (- (player-x player) (floor (/ *screen-width* 2)))))
        (abs-y
          (+ 1 (- (player-y player) (floor (/ *screen-height* 2))))))
    (loop :for i 
          :from 1 
          :below *screen-height* 
          :do
          (loop :for j 
                :from 2 
                :below *screen-width* 
                :do
                (let ((chr  (num-to-char (perlin-lookup (+ abs-x  i)
                                                        (+ abs-y  j)))  )) 
                  (write-at-point  chr j i
                                   (color-switch chr)))))))


(defun color-switch (chr)
  (case chr
    ((#\`) +magenta+)
    ((#\*) +blue+)
    ((#\~) +blue+)
    ((#\.) +green+)
    ((#\o) +red+)
    ((#\+) +yellow+)
    (t +white+)))



(defun draw-player ()
  (write-at-point 
    #\@
    (floor (/ *screen-width* 2))
    (floor (/ *screen-height* 2))
    +magenta+))


(defparameter *grid* nil)
(defparameter *world-width*  1000)
(defparameter *world-height*  1000)

(setf *grid* (perlin2d-grid   *world-width* *world-height*  0.10 5))

(defun num-to-char (num)
  (cond
    ((= num  0 ) #\Space)
    ((< num .2 ) #\~)
    ((< num .4 ) #\~)
    ((< num .5 ) #\~)
    ((< num .7 ) #\.)
    ((< num .8 ) #\+)
    ((< num .9 ) #\+)
    (t #\$)))


(defun move-down (player)
  (when (> 1043 (player-x player)) (incf (player-x player))))
(defun move-up (player)
  (when (< 45 (player-x player)) (decf (player-x player))))
(defun move-left (player)
  (when (< -45 (player-y player)) (decf (player-y player))))
(defun move-right (player)
  (when (> 953 (player-y player)) (incf (player-y player))))



(defscreen start
           :input ( ((nil) nil)
                    (t (quit-screen)))
           :output ((write-at-center "Welcome to the game.  Press any button."))
           :next 'play
           :boxed t)


(defscreen play
           :before ((init-player))
           :input ( ((nil) nil)
                    ((#\j) (move-down *player*))
                    ((#\k) (move-up *player*))
                    ((#\h) (move-left *player*))
                    ((#\l) (move-right *player*))
                    ((#\w) (run-screen (gethash 'win *screens*)))
                    (t (quit-screen)))
           :output ((refresh-window *standard-window*)
                    (draw-map *player*)
                    (draw-hud) 
                    (draw-player))
           :next 'lose
           :boxed t

           )

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

;(main)
