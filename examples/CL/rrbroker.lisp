;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Simple request-reply broker in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rrbroker
  (:nicknames #:rrbroker)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.rrbroker)

(defun main ()
  ;; Prepare our context and sockets
  (zmq:with-context (context 1)
    (zmq:with-socket (frontend context zmq:router)
      (zmq:with-socket (backend context zmq:dealer)
        (zmq:bind frontend "tcp://*:5559")
        (zmq:bind backend  "tcp://*:5560")

        ;; Initialize poll set
        (zmq:with-polls ((items . ((frontend . zmq:pollin)
                                   (backend  . zmq:pollin))))
          ;; Switch messages between sockets
          (loop
            (let ((revents (zmq:poll items)))
              (when (= (first revents) zmq:pollin)
                (loop
                  ;; Process all parts of the message
                 (let ((message (make-instance 'zmq:msg)))
                   (zmq:recv frontend message)

                   (if (not (zerop (zmq:getsockopt frontend zmq:rcvmore)))
                       (zmq:send backend message zmq:sndmore)
                       (progn
                         (zmq:send backend message 0)
                         ;; Last message part
                         (return))))))

              (when (= (second revents) zmq:pollin)
                (loop
                  ;; Process all parts of the message
                 (let ((message (make-instance 'zmq:msg)))
                   (zmq:recv backend message)

                   (if (not (zerop (zmq:getsockopt backend zmq:rcvmore)))
                       (zmq:send frontend message zmq:sndmore)
                       (progn
                         (zmq:send frontend message 0)
                         ;; Last message part
                         (return))))))))))))

  (cleanup))
