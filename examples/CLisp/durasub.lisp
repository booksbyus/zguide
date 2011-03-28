;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Durable subscriber in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.durasub
  (:nicknames #:durasub)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.durasub)

(defun main ()
  (zmq:with-context (context 1)
    ;; Connect our subscriber socket
    (zmq:with-socket (subscriber context zmq:sub)
      (zmq:setsockopt subscriber zmq:identity "Hello")
      (zmq:setsockopt subscriber zmq:subscribe "")
      (zmq:connect subscriber "tcp://localhost:5565")

      ;; Synchronize with publisher
      (zmq:with-socket (sync context zmq:push)
        (zmq:connect sync "tcp://localhost:5564")
        (send-text sync "")

        ;; Get updates, expect random Ctrl-C death
        (loop
          (let ((string (recv-text subscriber)))
            (message "~A~%" string)

            (when (string= string "END")
              (return)))))))

  (cleanup))
