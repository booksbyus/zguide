;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Cross-connected ROUTER sockets addressing each other in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rtrouter
  (:nicknames #:rtrouter)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.rtrouter)

(defun main ()
  (zmq:with-context (context 1)
    (zmq:with-socket (worker context zmq:router)
      (zmq:setsockopt worker zmq:identity "WORKER")
      (zmq:bind worker "ipc://rtrouter.ipc")

      (zmq:with-socket (server context zmq:router)
        (zmq:setsockopt server zmq:identity "SERVER")
        (zmq:connect server "ipc://rtrouter.ipc")

        (sleep 1)

        (send-more-text server "WORKER")
        (send-more-text server "")
        (send-text server "send to worker")
        (dump-socket worker)

        (send-more-text worker "SERVER")
        (send-more-text worker "")
        (send-text worker "send to server")
        (dump-socket server))))

  (cleanup))
