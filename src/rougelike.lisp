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

(defclass screen ()
  ((window 
     :initarg :window
     :initform *standard-window*)) )

(defclass lose-screen (screen)
  (window))

(defclass win-screen (screen)
  (window))

(defgeneric get-input (screen)
  (:documentation "collect input"))

(defgeneric draw-screen (screen)
  (:documentation "render output"))


(defmethod draw-screen ((screen win-screen))
  (write-at-point "i am the win screen" 0 0))

(defmethod draw-screen ((screen lose-screen))
  (write-at-point "i am the lose screen" 0 0))


(defmethod get-input ((screen lose-screen))
      (let  ((c (get-char *standard-window* :ignore-error t)))
        (case c
          ((nil) nil)
          ((#\q) (setf *running* nil))
          ((#\t) (write-at-point "lose screen input" 3 3)))))

(defmethod get-input ((screen win-screen))
      (let  ((c (get-char *standard-window* :ignore-error t)))
        (case c
          ((nil) nil)
          ((#\q) (setf *running* nil))
          ((#\t) (write-at-point "win screen input" 3 3)))))



(defun run-screen (screen)
  (clear-screen)
  (setf *running* t)
    (loop :while *running*
          :do
          (refresh-window *standard-window*)
          (sleep *interval*  )
          (get-input screen )
          (draw-screen screen )))


(defparameter *lose* nil)
(defparameter *win* nil)

(defun main ()
  (with-init
    (setf *win* (make-instance 'win-screen))
    (setf *lose* (make-instance 'lose-screen))
    (run-screen *win*)
    (run-screen *lose*)
    ))

(main)
