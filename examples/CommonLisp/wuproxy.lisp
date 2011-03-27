;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Weather proxy device in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.wuproxy
  (:nicknames #:wuproxy)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.wuproxy)

(defun main ()
  (zmq:with-context (context 1)
    ;; This is where the weather server sits
    (zmq:with-socket (frontend context zmq:sub)
      (zmq:connect frontend "tcp://192.168.55.210:5556")

      ;; This is our public endpoint for subscribers
      (zmq:with-socket (backend context zmq:pub)
        (zmq:bind backend "tcp://10.1.1.0:8100")

        ;; Subscribe on everything
        (zmq:setsockopt frontend zmq:subscribe "")

        ;; Shunt messages out to our own subscribers
        (loop
          (loop
            ;; Process all parts of the message
           (let ((message (make-instance 'zmq:msg)))
             (zmq:recv frontend message)

             (if (not (zerop (zmq:getsockopt frontend zmq:rcvmore)))
                 (zmq:send backend message zmq:sndmore)
                 (progn
                   (zmq:send backend message 0)
                   ;; Last message part
                   (return)))))))))

  (cleanup))
