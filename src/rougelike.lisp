(in-package :cl-user)
(ql:quickload "cl-charms")
(ql:quickload "perlin")
(defpackage rougelike
  (:use :cl
        :cl-charms
        :perlin))
(in-package :rougelike)

(defparameter *screen-width* 120)
(defparameter *screen-height* 30)

(defun init ()
  (with-curses ()
    (disable-echoing)
    (cl-charms/low-level:curs-set 0)
    (enable-raw-input :interpret-control-characters t)
    (enable-non-blocking-mode *standard-window*)
    (cl-charms/low-level:start-color) ))


(defclass ui ()
    ((window 
      :initarg :window
      :initform *standard-window*)) )

(defclass start (ui)
  (window))

(defclass win (ui)
  (window))

(defclass lose (ui)
  (window))

(defgeneric draw-ui (ui)
  (:documentation "draw the given ui on the screen"))

(defmethod draw-ui ((ui start))
  (write-string-at-point *standard-window* 
                         "i am the start screen"
                         0 0))

(defmethod draw-ui ((ui win))
  (write-string-at-point *standard-window* 
                         "i am the win screen"
                         0 0))

(defmethod draw-ui ((ui lose))
  (write-string-at-point *standard-window* 
                         "i am the lose screen"
                         0 0))


(defun draw-game (ui)
  (draw-ui ui))

(defparameter l ())

(defun main ()
  (init)
  (setf l (make-instance 'lose))
  (draw-game l))

(main)
