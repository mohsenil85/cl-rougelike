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
(defparameter *monsters* nil)

(defstruct tile x y glyph color)
(defstruct (player 
             (:include tile)) 
                health
                
                )
(defstruct (monster (:include player)))

(defun init-player ()
  (setf *player* (make-player 
                   :x 0 ;(random *map-size*)
                   :y 0 ;(random *map-size*)
                   :glyph #\@
                   :color +magenta+
                   :health 10)))

(defparameter *monsters* nil)

(defun init-monster ()
  (push   (make-monster 
                   :x  (random *map-size*)
                   :y  (random *map-size*)
                   :glyph #\M
                   :color +yellow+
                   :health 10) *monsters*) )

(defun init-monsters ()
  (loop for i  below 10 do 
        (init-monster)))


(setf *seed* (random 10))

(defun draw-hud ()
  (write-at-point 
    (format nil "X: ~A Y: ~A Monsters: ~A "
            (player-x *player*)
            (player-y *player*)
            (length *monsters*)
            )
    1 1 1)
  )

(defun draw-monster (monster player)
  (let ((monst-x (- (floor (/ *screen-width* 2))
                    (- (player-x player) (monster-x monster))))
        (monst-y (- (floor (/ *screen-height* 2)) 
                    (- (player-y player)(monster-y monster)))   ))
    (if (and 
          (< 1 monst-x *screen-width*)
          (< 1 monst-y *screen-height*)) 
      (write-at-point (monster-glyph monster ) monst-x monst-y ))))


(defun draw-monsters (monsters player)
  (loop for m in  monsters do
        (draw-monster m player)))

(defun player-coords-to-screen (player x-offset y-offset)
  (let ((x (+ x-offset (player-x player)))
        (y (+ y-offset (player-y player))))
    (draw-tile  (create-map-tile x y)
                (+ x-offset (floor (/ *screen-width* 2)))
                (+ y-offset (floor (/ *screen-height* 2))))))

(defun draw-map (player)
  (let ((x-min (1+ (* -1 (floor (/ *screen-width* 2)))))
        (x-max (floor (/ *screen-width* 2)))
        (y-min (1+ (* -1 (floor (/ *screen-height* 2)))))
        (y-max ( floor (/ *screen-height* 2))))
    (loop for i from x-min below x-max do
        (loop for j from y-min below y-max do
              ( player-coords-to-screen player i j)))))

(defparameter *perlin-depth* 4)
(defparameter *perlin-freq* .1)

(defun create-glyph (x y)
  (if (and (<= 0 y)
           (<= 0 x)
           (>= *map-size* y)
           (>= *map-size* x)) 
    (num-to-char 
      (perlin2d x y 
                *perlin-freq* 
                *perlin-depth*))
  #\Space))


(defun create-map-tile (x y)
  (let ((glyph (create-glyph x y)))
    (make-tile :x x
               :y y
               :glyph glyph
               :color (color-switch glyph))))

(defun draw-tile (tile x y)
  (write-at-point (tile-glyph tile) x y
                  (tile-color tile)))




(defun color-switch (chr)
  (case chr
    ((#\`) +magenta+)
    ((#\*) +blue+)
    ((#\~) +blue+)
    ((#\.) +green+)
    ((#\o) +red+)
    ((#\+) +yellow+)
    (t +white+)))



(defun draw-player (player)
  (draw-tile player
    (floor (/ *screen-width* 2))
    (floor (/ *screen-height* 2))))


(defun num-to-char (num)
  (cond
    ((= num  0 ) #\Space)
    ((< num .2 ) #\`)
    ((< num .4 ) #\~)
    ((< num .5 ) #\~)
    ((< num .7 ) #\.)
    ((< num .8 ) #\+)
    ((< num .9 ) #\+)
    (t #\$)))


(defparameter *map-size* 100)

(defun move-down (player)
    (when (> *map-size* (player-y player)) 
      (incf (player-y player))))
(defun move-up (player)
    (when (< 0 (player-y player))
      (decf (player-y player))))
(defun move-left (player)
  ( when (< 0 (player-x player))
    (decf (player-x player))))
(defun move-right (player)
  ( when (> *map-size* (player-x player)) 
    (incf (player-x player))))



(defscreen start
           :input ( ((nil) nil)
                    (t (quit-screen)))
           :output ((write-at-center "Welcome to the game.  Press any button."))
           :next 'play
           :boxed t)


(defscreen play
           :before ((init-player)
                     (init-monsters))
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
                 ;   (draw-monster *monster*) 
                    (draw-player *player*)
                    (draw-monsters *monsters* *player*)
                    )
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

(main)
