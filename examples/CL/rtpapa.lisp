;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Custom routing Router to Papa (ROUTER to REP) in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rtpapa
  (:nicknames #:rtpapa)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.rtpapa)

;; We will do this all in one thread to emphasize the sequence of events...
(defun main ()
  (zmq:with-context (context 1)
    (zmq:with-socket (client context zmq:ROUTER)
      (zmq:bind client "ipc://routing.ipc")
      (zmq:with-socket (worker context zmq:rep)
        (zmq:setsockopt worker zmq:identity "A")
        (zmq:connect worker "ipc://routing.ipc")

        ;; Wait for sockets to stabilize
        (sleep 1)

        ;; Send papa address, address stack, empty part, and request
        (send-more-text client "A")
        (send-more-text client "address 3")
        (send-more-text client "address 2")
        (send-more-text client "address 1")
        (send-more-text client "")
        (send-text client "This is the workload")

        ;; Worker should get just the workload
        (dump-socket worker)

        ;; We don't play with envelopes in the worker
        (send-text worker "This is the reply")

        ;; Now dump what we got off the ROUTER socket...
        (dump-socket client))))

  (cleanup))
