;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Hello World server in Common Lisp
;;;  Binds REP socket to tcp://*:5555
;;;  Expects "Hello" from client, replies with "World"
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rrserver
  (:nicknames #:rrserver)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.rrserver)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket to talk to clients
    (zmq:with-socket (responder context zmq:rep)
      (zmq:connect responder "tcp://localhost:5560")

      (loop
        (let ((request (make-instance 'zmq:msg)))
          ;; Wait for next request from client
          (zmq:recv responder request)
          (message "Received request: [~A]~%"
                   (zmq:msg-data-as-string request))

          ;; Do some 'work'
          (sleep 1)

          ;; Send reply back to client
          (let ((reply (make-instance 'zmq:msg :data "World")))
            (zmq:send responder reply))))))

  (cleanup))
