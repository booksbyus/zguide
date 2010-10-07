;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Publisher for durable subscriber in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.durapub2
  (:nicknames #:durapub2)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.durapub2)

(defun main ()
  (zmq:with-context (context 1)
    ;; Subscriber tells us when it's ready here
    (zmq:with-socket (sync context zmq:pull)
      (zmq:bind sync "tcp://*:5564")
      ;; We send updates via this socket
      (zmq:with-socket (publisher context zmq:pub)
        (zmq:bind publisher "tcp://*:5565")

        ;; Prevent publisher overflow from slow subscribers
        (zmq:setsockopt publisher zmq:hwm 1)

        ;; Specify swap space in bytes, this covers all subscribers
        (zmq:setsockopt publisher zmq:swap 25000000)

        ;; Wait for synchronization request
        (recv-text sync)

        ;; Now broadcast exactly 10 updates with pause
        (dotimes (update-nbr 10)
          (send-text publisher (format nil "Update ~D" update-nbr))
          (sleep 1))
        (send-text publisher "END")))

    ;; Give 0MQ/2.0.x time to flush output
    (sleep 1))

  (cleanup))
