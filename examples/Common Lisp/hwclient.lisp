;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Hello World client in Common Lisp
;;;  Connects REQ socket to tcp://localhost:5555
;;;  Sends "Hello" to server, expects "World" back
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.hwclient
  (:nicknames #:hwclient)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.hwclient)

(defun main ()
  ;; Prepare our context and socket
  (zmq:with-context (context 1)
    (zmq:with-socket (socket context zmq:req)
      (message "Connecting to hello world server...~%")
      (zmq:connect socket "tcp://localhost:5555")

      ;; Do 10 requests, waiting each time for a response
      (dotimes (request-nbr 10)
        (let ((request (make-instance 'zmq:msg :data "Hello")))
          (message "Sending request ~D...~%" request-nbr)
          (zmq:send socket request))

        ;; Get the reply
        (let ((response (make-instance 'zmq:msg)))
          (zmq:recv socket response)
          (message "Received reply ~D: [~A]~%"
                   request-nbr (zmq:msg-data-as-string response))))))

  (cleanup))
