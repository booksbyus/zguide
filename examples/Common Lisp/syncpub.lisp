;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Synchronized publisher in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.syncpub
  (:nicknames #:syncpub)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.syncpub)

;; We wait for 10 subscribers
(defparameter *expected-subscribers* 10)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket to talk to clients
    (zmq:with-socket (publisher context zmq:pub)
      (zmq:bind publisher "tcp://*:5561")
      ;; Socket to receive signals
      (zmq:with-socket (syncservice context zmq:rep)
        (zmq:bind syncservice "tcp://*:5562")

        ;; Get synchronization from subscribers
        (loop :repeat *expected-subscribers* :do
          ;; - wait for synchronization request
          (let ((msg (make-instance 'zmq:msg)))
            (zmq:recv syncservice msg))
          ;; - send synchronization reply
          (let ((msg (make-instance 'zmq:msg :data "")))
            (zmq:send syncservice msg)))

        ;; Now broadcast exactly 1M updates followed by END
        (loop :repeat 1000000 :do
          (let ((msg (make-instance 'zmq:msg :data "Rhubarb")))
            (zmq:send publisher msg)))
        (let ((msg (make-instance 'zmq:msg :data "END")))
          (zmq:send publisher msg))))

    ;; Give 0MQ/2.0.x time to flush output
    (sleep 1))

  (cleanup))
