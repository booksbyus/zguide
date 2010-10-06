;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Task sink in Common Lisp
;;;  Binds PULL socket to tcp://localhost:5558
;;;  Collects results from workers via that socket
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.tasksink
  (:nicknames #:tasksink)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.tasksink)

(defun main ()
  ;; Prepare our context and socket
  (zmq:with-context (context 1)
    (zmq:with-socket (receiver context zmq:pull)
      (zmq:bind receiver "tcp://*:5558")

      ;; Wait for start of batch
      (let ((msg (make-instance 'zmq:msg)))
        (zmq:recv receiver msg))

      ;; Start our clock now
      (let ((elapsed-time
             (with-stopwatch
               (dotimes (task-nbr 100)
                 (let ((msg (make-instance 'zmq:msg)))
                   (zmq:recv receiver msg)
                   (let ((string (zmq:msg-data-as-string msg)))
                     (declare (ignore string))

                     (if (= 1 (denominator (/ task-nbr 10)))
                         (message ":")
                         (message "."))))))))

        ;; Calculate and report duration of batch
        (message "Total elapsed time: ~F msec~%" (/ elapsed-time 1000.0)))))

  (cleanup))
