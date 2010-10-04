;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Task sink
;;;  Binds PULL socket to tcp://localhost:5558
;;;  Collects results from workers via that socket
;;;
;;; 'with-stopwatch' macro is taken from 'cl-zmq'
;;; by Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.tasksink
  (:nicknames #:tasksink)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.tasksink)

(defmacro with-stopwatch (&body body)
  (let ((sec0 (gensym))
        (sec1 (gensym))
        (usec0 (gensym))
        (usec1 (gensym)))
    `(multiple-value-bind (,sec0 ,usec0)
         (isys:gettimeofday)
       (unwind-protect
            (progn ,@body))
       (multiple-value-bind (,sec1 ,usec1)
           (isys:gettimeofday)
         (+ (* 1e6 (- ,sec1 ,sec0))
            ,usec1 (- ,usec0))))))

(defun main ()
  ;; Prepare our context and socket
  (zmq:with-context (context 1)
    (zmq:with-socket (receiver context zmq:pull)
      (zmq:bind receiver "tcp://*:5558")

      ;; Wait for start of batch
      (let ((msg (make-instance 'zmq:msg)))
        (zmq:recv receiver msg)
        (let ((string (zmq:msg-data-as-string msg)))
          ;; Start our clock now
          (let ((elapsed-time
                 (with-stopwatch
                   (dotimes (task-nbr 100)
                     (zmq:recv receiver msg)
                     (setf string (zmq:msg-data-as-string msg))

                     (if (= 1 (denominator (/ task-nbr 10)))
                         (message ":")
                         (message "."))))))

            ;; Calculate and report duration of batch
            (message "Total elapsed time: ~F msec~%" (/ elapsed-time 1000.0)))))))

  (cleanup))
