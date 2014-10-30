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
                  color))

(defparameter *player* nil)
(defstruct player x y health)

(defun init-player ()
  (setf *player* (make-player 
                   :x (random *world-width*)
                   :y (random *world-height*)
                   )))

(defun calc-origin-from-player-coords (x y)
  (values 
    (- (- y (floor (/ *screen-height*  2))) 27)
    (- (- x (floor (/  *screen-width*  2))) 32 )
    ))

(defun perlin-lookup (x y)
  (aref *grid* y x))


(defun get-origin-relative-player (p)
   (values
      (+ 1 (- (player-x p) (floor (/ *screen-width* 2))))  
      (+ 1 (- (player-y p) (floor (/ *screen-height* 2))))
      ))

(defun get-map-around-player (player) 
  (multiple-value-bind (abs-x abs-y)
  (get-origin-relative-player player)
  (loop for i below *screen-height* do
        (loop for j below *screen-width* do
              (write-at-point  (num-to-char (perlin-lookup (+ abs-x  i)
                             (+ abs-y  j)))
                               i j)))))



(defun get-tile-above-player (p)
  (num-to-char 
    (perlin-lookup 
      (- (player-y p) 1)
      (player-x p)
    ))
  )


;(draw-tile)

;(init-player)
;*player*
;(get-origin-relative-player *player*)
;(get-abs-coords *player*)
(defun get-abs-coords (p)
  (values
    (player-x p)
    (player-y p)
    )
  )
;(get-tile-above-player *player*)

(defun myfun ()
  (draw-map-cell (get-origin-tile *player*) 
                 1 1 
                 ))

(defun draw-player ()
  (write-at-point 
    #\@
    (floor (/ *screen-width* 2))
    (floor (/ *screen-height* 2))
    +magenta+))


(defparameter *grid* nil)
(defparameter *world-width*  1000)
(defparameter *world-height*  1000)

(setf *grid* (perlin2d-grid   *world-width* *world-height*  0.1 4))

(defun num-to-char (num)
  (cond
    ((< num .6 ) #\~)
    ;((< num .6 ) #\o)
    ((< num .8 ) #\.)
    ;((< num .8 ) #\()
    ((< num .9 ) #\+)
    (t #\$)))

(defun draw-map-cell (chr i j)
    (case chr
      ((#\~) (write-at-point chr i j +blue+ ))
      ((#\.) (write-at-point chr i j +red+ ))
      ((#\o) (write-at-point chr i j +red+ ))
      ((#\() (write-at-point chr i j +green+))
      ((#\+) (write-at-point chr i j +green+ ))
      (otherwise (write-at-point  chr i j))))

(defun draw-map (player)
  (loop for i below *screen-width* do
        (loop for j below *screen-height* do
              (draw-map-cell player i j) )))

;(defun draw-map (player)
;  (multiple-value-bind (x-zero y-zero)
;    (calc-origin-from-player-coords (player-x player) (player-y player))
;    (let ((max-x (+ (- *screen-width* 2) x-zero))
;          (max-y (+ (- *screen-height* 1) y-zero)) )
;      (loop for i from x-zero below max-x do
;            (loop for j from y-zero below max-y do
;                  (draw-map-cell player i j) ) ) )))

(defun move-down (player)
  (decf (player-x player)))



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
                    ((#\w) (run-screen (gethash 'win *screens*)))
                    (t (quit-screen)))
           :output ((refresh-window *standard-window*)
                    (get-map-around-player *player*)
                    ;(draw-map *player*) 
                    (draw-player))
           :next 'lose
          ; :boxed t
           
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
