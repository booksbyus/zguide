;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Simple message queuing broker in Common Lisp
;;;  Same as request-reply broker but using QUEUE device
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.msgqueue
  (:nicknames #:msgqueue)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.msgqueue)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket facing clients
    (zmq:with-socket (frontend context zmq:router)
      (zmq:bind frontend "tcp://*:5559")
      ;; Socket facing services
      (zmq:with-socket (backend context zmq:dealer)
        (zmq:bind backend  "tcp://*:5560")

        ;; Start built-in device
        (zmq:device zmq:queue frontend backend))))

  (cleanup))
