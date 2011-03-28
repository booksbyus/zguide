;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Pubsub envelope subscriber in Common Lisp
;;;  Note that the zhelpers package also provides recv-text
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.psenvsub
  (:nicknames #:psenvsub)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.psenvsub)

(defun main ()
  ;; Prepare our context and publisher
  (zmq:with-context (context 1)
    (zmq:with-socket (subscriber context zmq:sub)
      (zmq:connect subscriber "tcp://localhost:5563")
      (zmq:setsockopt subscriber zmq:subscribe "B")

      (loop
        ;; Read envelope with address
        (let ((address (recv-text subscriber)))
          ;; Read message contents
          (let ((contents (recv-text subscriber)))
            (message "[~A] ~A~%" address contents))))))

  (cleanup))
