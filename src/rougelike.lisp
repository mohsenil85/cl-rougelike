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

(defgeneric next-screen (screen)
  (:documentation "returns the next screen to be run"))


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


(defun make-start-screen ()
  (progn
    (defclass start-screen (screen) 
      (window))

    (defmethod draw-screen ((screen start-screen))
      (write-at-point "i am the start screen" 0 0))

    (defmethod get-input ((screen start-screen))
      (let  ((c (get-char *standard-window* :ignore-error t)))
        (case c
          ((nil) nil)
          ((#\q) (setf *running* nil))
          ((#\t) (write-at-point "start screen input" 3 3)))) )

    (defmethod next-screen ((screen start-screen))
      (gethash 'win *screens*))

    (setf (gethash 'start *screens*)  (make-instance 'start-screen))))

(defmacro defscreen (scrn )
  `(funcall #'(lambda (s) 
               (progn

                 (defclass s (screen)
                   (window))

                 (defmethod draw-screen ((screen s))
                   (write-at-point "i am the start screen" 0 0))

                 (defmethod get-input ((screen s))
                   (let  ((c (get-char *standard-window* :ignore-error t)))
                     (case c
                       ((nil) nil)
                       ((#\q) (setf *running* nil))
                       ((#\t) (write-at-point "start screen input" 3 3))))  )

                 (defmethod next-screen ((screen s))
                   (gethash 'win *screens*))

                 (setf (gethash ',scrn *screens*)  (make-instance ',scrn)))) 
            ,scrn))

(macroexpand-1 '(defscreen start-screen))


(defmethod next-screen ((screen lose-screen))
  nil)

(defmethod next-screen ((screen win-screen))
  (gethash 'lose *screens*))


(defun run-screen (screen)
  (if screen
    (progn (clear-screen)
           (setf *running* t)
           (loop :while *running*
                 :do
                 (refresh-window *standard-window*)
                 (sleep *interval*  )
                 (get-input screen )
                 (draw-screen screen ))
           (run-screen (next-screen screen)))
    (sb-sys:os-exit 0)))


(defun main ()
  (with-init
    (make-start-screen)
    (defscreen start-screen)
    (setf (gethash 'win *screens*)  (make-instance 'win-screen))
    (setf (gethash 'lose *screens*)  (make-instance 'lose-screen))
    (run-screen (gethash 'start *screens*))
    ))

(main)

(defparameter *test2* nil)
(defparameter *test* nil)

(defmacro testmac (foo &body body)
  `(funcall #'(lambda (x) 
                (progn 
                  (setf *test* x)
                  ,@body
                  
                  )) ,foo))

*test*
*test2*
(testmac "blah" (setf *test2* "foo" ))

;(setq myh (make-hash-table))
;
;(gethash 'foo myh)
;(setf (gethash 'foo myh) "my foo")
;(setf (gethash 'f2oo myh) "my foo2")
;
;(maphash #'(lambda (key val) (format t "~A ~A ~%" key val )) myh)
