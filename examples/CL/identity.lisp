;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Demonstrate identities as used by the request-reply pattern in Common Lisp.
;;;  Run this program by itself.  Note that the utility functions are
;;;  provided by zhelpers.lisp.  It gets boring for everyone to keep repeating
;;;  this code.
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.identity
  (:nicknames #:identity)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.identity)

(defun main ()
  (zmq:with-context (context 1)
    (zmq:with-socket (sink context zmq:router)
      (zmq:bind sink "inproc://example")

      ;; First allow 0MQ to set the identity
      (zmq:with-socket (anonymous context zmq:req)
        (zmq:connect anonymous "inproc://example")
        (send-text anonymous "ROUTER uses a generated UUID")
        (dump-socket sink)

        ;; Then set the identity ourself
        (zmq:with-socket (identified context zmq:req)
          (zmq:setsockopt identified zmq:identity "Hello")
          (zmq:connect identified "inproc://example")
          (send-text identified "ROUTER socket uses REQ's socket identity")
          (dump-socket sink)))))

  (cleanup))
