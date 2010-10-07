;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Hello World client in Common Lisp
;;;  Connects REQ socket to tcp://localhost:5555
;;;  Sends "Hello" to server, expects "World" back
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rrclient
  (:nicknames #:rrclient)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.rrclient)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket to talk to server
    (zmq:with-socket (requester context zmq:req)
      (zmq:connect requester "tcp://localhost:5559")

      (dotimes (request-nbr 10)
        (let ((request (make-instance 'zmq:msg :data "Hello")))
          (zmq:send requester request))

        (let ((response (make-instance 'zmq:msg)))
          (zmq:recv requester response)
          (message "Received reply ~D: [~A]~%"
                   request-nbr (zmq:msg-data-as-string response))))))

  (cleanup))
