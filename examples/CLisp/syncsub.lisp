;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Synchronized subscriber in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.syncsub
  (:nicknames #:syncsub)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.syncsub)

(defun main ()
  (zmq:with-context (context 1)
    ;; First, connect our subscriber socket
    (zmq:with-socket (subscriber context zmq:sub)
      (zmq:connect subscriber "tcp://localhost:5561")
      (zmq:setsockopt subscriber zmq:subscribe "")

      ;; Second, synchronize with publisher
      (zmq:with-socket (syncclient context zmq:req)
        (zmq:connect syncclient "tcp://localhost:5562")

        ;; - send a synchronization request
        (let ((msg (make-instance 'zmq:msg :data "")))
          (zmq:send syncclient msg))

        ;; - wait for synchronization reply
        (let ((msg (make-instance 'zmq:msg)))
          (zmq:recv syncclient msg))

        ;; Third, get our updates and report how many we got
        (let ((updates 0))
          (loop
            (let ((msg (make-instance 'zmq:msg)))
              (zmq:recv subscriber msg)
              (when (string= "END" (zmq:msg-data-as-string msg))
                (return))
              (incf updates)))

          (message "Received ~D updates~%" updates)))))

  (cleanup))
