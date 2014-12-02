(in-package :cl-user)

;;;;
;;;so, 2 pieces of state:  "game" where 
;{
;   game :
;     [
;       player:
;       { 
;        x : 44
;        y: 33
;       },
;       monster:
;       {
;        x: 22
;        y: 44
;       }
;     ]
;}
;
;and "uis"  like (draw ui game) (handle-input ui game) 
;
;;;  both take game as a parameter, and mutate it?
;;;  ;;  i guess it makes sense that after handling input, the game state would be different
;;;  ;;;  so would we end up with code like (setf game (handleinput ui game))?  
;;;  draw ui would be pure.  ui doesn't even have to be a parameter. it would just be 
;;;  (draw-ui game) or maybe (render-ui state)
;;;  i guess that's why they are generaic or should be.  draw ui will specialize in ui type (map, inventory, win lose etc)
;;;  
;;;  ;;state would be what's saved into a database
;;;
;;;  a list of uis, that's what keeps track of ui state.  
;;;  
;;;  the loop goes:
;;  (defun run-game (ui game)
;;;   (render ui)
;;;   (gatherinput)
;;;   (mutate game) ;; that is where we would do non pure shit to the game object/hash
;;;   (run-game ((next-ui ui) (game)) ;; recur here with next game iteration
;;;   
;;;   
;;;   
;;;   ;;; so game could have a next-ui property?
;;;   or a global *cuurrent-ui* var


;;;;  luck instead of mana..  but it could be negative...  randomly good or bad  so you use 3 luck to open a door, but instead it spawns an imp...  then you can get some sort of meta-luck 
;;;; to make it good more often

;; maybe luck AND mana
(ql:quickload "cl-charms")
(ql:quickload "perlin")
(ql:quickload "omens")
(defpackage rougelike
  (:use :cl
        :cl-charms
        :perlin
        :omens))
(in-package :rougelike)

(defun coords-to-glyph (x y)
  (if (and (<= 0 y)
           (<= 0 x)
           (>= *map-size* y)
           (>= *map-size* x)) 
    (num-to-char 
      (perlin2d x y 
                *perlin-freq* 
                *perlin-depth*))
  #\Space))

(defparameter *player* nil)
(defparameter *monsters* nil)

(defstruct tile x y glyph color)
(defstruct (player (:include tile)) 
                health)
(defstruct (monster (:include player)))

(defclass tile () 
  ((x :accessor :object-x)
   (y :accessor :object-y)
   (glyph :accessor :tile-glyph)
   (color :accessor :tile-color)
   ))

(defclass water-tile (tile)
  (glyph :accessor :tile-glyph
         :init-form #\~)
   (color :accessor :tile-color
          :initform +blue+
          )
  )

(defclass object ()
  ((x :accessor :object-x)
   (y :accessor :object-y)))

;;;;;entity -> player -> monster
;;;
;;;     also map, map-tile, hud
;;;init draw move
;;;
;;;but also have monsters and monster...  
;;;push monsters et al into *state* var.  player as well..  hud?
;;;
;;;
;;;want to do (for all obj in *objects do (draw object))
;;;and (all obj in objects do update object

(defun init-player ()
  (make-player 
                   :x 0 ;(random *map-size*)
                   :y 0 ;(random *map-size*)
                   :glyph #\@
                   :color +magenta+
                   :health 10))

(defparameter *monsters* nil)

(defun init-monster ()
  (make-monster 
                   :x 3 ;(random *map-size*)
                   :y 4 ;(random *map-size*)
                   :glyph #\M
                   :color +yellow+
                   :health 10 ))

(defun init-tile (x y)
  (let ((glyph (coords-to-glyph x y)))
    (make-tile :x x
               :y y
               :glyph glyph
               :color (color-switch glyph))))


(defclass node-object ()
  ((x :accessor world-x :initarg :x)
   (y :accessor world-y :initarg :y)
   (glyph :accessor :node-glyph :initarg :glyph)
   (color :accessor :node-color :initform +white+ :initarg :color)))
;
(defclass player-new (node-object) ((health :initform 10 :accessor health)))

(defparameter *state* (make-hash-table)) 

;(defgeneric init-object ((node-object n))
;  (:documentation "initzialize the node object"))


(defun add-player-to-state (player)
  (setf (gethash player *state* ) player ))

(setf (gethash *player* *state*) :foo)
(gethash *player* *state* )


(defparameter *player* (make-instance 'player-new
                                      :x 0 
                                      :y 0 
                                      :glyph #\@ :color +yellow+   
                                      ))
*player*
*state*
(add-player-to-state *player*)

;*state*

;;could have a node object that had x, , glyph, color, other props, indexed by #'eql name

()


(defun init-world (state)
  "functional method to push all game objects into state returns new state"
  (push (init-player) state)
  (loop for i  below 10 do 
        (push (init-monster) state))
  state
  )

(defun update-world (state)
  "imperitve method to update objects in state, returns new state"
  ;;for s in state, do update state
  state
  )

(setf *state* (init-world *state*))

(defun init-monsters ()
  (loop for i  below 10 do 
        (push (init-monster) *monsters*)))


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

;;x-offset is distance from the window edge (in screen coords, not map coords)
;;factor out init-tile, just do drawing


;;call to monster-x below should be (world-x object)

 (defun find-screen-coods (player x-offset y-offset object)
   (values ((monster-x monster) (+ x-offset (player-x player)))
        ((monster-y monster) (+ y-offset (player-y player)))))

;(init-player)
;(init-monster )

;*monsters*


;(find-screen-coods (*player* 5 5 ()))

(defun player-coords-to-screen-coords (player x-offset y-offset object)
  "change map (or absolute) coords to screen coords"
  (let ((x (+ x-offset (player-x player)))
        (y (+ y-offset (player-y player))))
    (draw-tile 
                (+ x-offset (floor (/ *screen-width* 2)))
                (+ y-offset (floor (/ *screen-height* 2))))))

(defun draw-around-player (player)
  "given a player's coords, draw a screen-sized section of map around him"
  (let ((x-min (1+ (* -1 (floor (/ *screen-width* 2)))))
        (x-max (floor (/ *screen-width* 2)))
        (y-min (1+ (* -1 (floor (/ *screen-height* 2)))))
        (y-max ( floor (/ *screen-height* 2))))
    (loop for i from x-min below x-max do
        (loop for j from y-min below y-max do
              ( player-coords-to-screen-coords player i j)))))

()

(defun draw-tile (tile player x y)
  ;; (init-tile x y)
  ;;x = i in the loop, y = j
  (player-coords-to-screen player x y)
  )

(defparameter *perlin-depth* 4)
(defparameter *perlin-freq* .1)




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
           :before ((setf *state* (init-world *state*)))
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

;(main)
