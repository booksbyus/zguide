;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Pubsub envelope publisher in Common Lisp
;;;  Note that the zhelpers package also provides send-text and send-more-text
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.psenvpub
  (:nicknames #:psenvpub)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.psenvpub)

(defun main ()
  ;; Prepare our context and publisher
  (zmq:with-context (context 1)
    (zmq:with-socket (publisher context zmq:pub)
      (zmq:bind publisher "tcp://*:5563")

      (loop
        ;; Write two messages, each with an envelope and content
        (send-more-text publisher "A")
        (send-text publisher "We don't want to see this")
        (send-more-text publisher "B")
        (send-text publisher "We would like to see this")
        (sleep 1))))

  (cleanup))
