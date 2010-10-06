;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Task sink - design 2 in Common Lisp
;;;  Binds PULL socket to tcp://localhost:5558
;;;  Collects results from workers via that socket
;;;  Adds pub-sub flow to send kill signal to workers
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.tasksink2
  (:nicknames #:tasksink2)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.tasksink2)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket to receive messages on
    (zmq:with-socket (receiver context zmq:pull)
      (zmq:bind receiver "tcp://*:5558")
      ;; Socket for worker control
      (zmq:with-socket (controller context zmq:pub)
        (zmq:bind controller "tcp://*:5559")
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
          (message "Total elapsed time: ~F msec~%" (/ elapsed-time 1000.0)))

        ;; Send kill signal to workers
        (let ((kill (make-instance 'zmq:msg :data "KILL")))
          (zmq:send controller kill))

        ;; Give 0MQ time to deliver
        (sleep 1))))

  (cleanup))
