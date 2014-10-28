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


(defparameter *screen-width* 120)
(defparameter *screen-height* 30)
(defparameter *interval* .01)
(defparameter *running* nil)
(defparameter *screens* (make-hash-table))

(defstruct screen input output next)

(defmacro defscreen (screen-name &key input output next)
  `(setf (gethash ',screen-name *screens*) 
         (make-screen 
           :input 
           '(let  ((c (get-char *standard-window* :ignore-error t)))
              (case c
                ,@input))

           :output ',@output

           :next '(gethash ,next *screens*))))

(defscreen win
           :input ( ((nil) nil)
                    ((#\q) (setf *running* nil))
                    ((#\t) (write-at-point "win screen input" 3 3)))
           :output ((write-at-point "i am the win screen" 0 0))
           :next 'lose)

(defscreen lose
           :input ( ((nil) nil)
                    ((#\q) (setf *running* nil))
                    ((#\t) (write-at-point "lose screen input" 3 3)))
           :output ((write-at-point "i am the lose screen" 0 0)))

(defun run-screen (screen)
  (if screen
    (progn (clear-screen)
           (setf *running* t)
           (loop :while *running*
                 :do
                 (refresh-window *standard-window*)
                 (sleep *interval*  )
                 (eval (screen-input screen ))
                 (eval (screen-output screen )))
           (run-screen (eval (screen-next screen))))
    (sb-sys:os-exit 0)))

(defun main ()
  (with-init
    (run-screen (gethash 'win *screens*))))

(main)
