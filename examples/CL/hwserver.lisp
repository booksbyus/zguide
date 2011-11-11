;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Hello World server in Common Lisp
;;;  Binds REP socket to tcp://*:5555
;;;  Expects "Hello" from client, replies with "World"
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.hwserver
  (:nicknames #:hwserver)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.hwserver)

(defun main ()
  ;; Prepare our context and socket
  (zmq:with-context (context 1)
    (zmq:with-socket (socket context zmq:rep)
      (zmq:bind socket "tcp://*:5555")

      (loop
        (let ((request (make-instance 'zmq:msg)))
          ;; Wait for next request from client
          (zmq:recv socket request)
          (message "Received request: [~A]~%"
                   (zmq:msg-data-as-string request))

          ;; Do some 'work'
          (sleep 1)

          ;; Send reply back to client
          (let ((reply (make-instance 'zmq:msg :data "World")))
            (zmq:send socket reply))))))

  (cleanup))
