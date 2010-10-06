;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Reading from multiple sockets in Common Lisp
;;;  This version uses zmq_poll()
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.mspoller
  (:nicknames #:mspoller)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.mspoller)

(defun main ()
  (zmq:with-context (context 1)
    ;; Connect to task ventilator
    (zmq:with-socket (receiver context zmq:pull)
      (zmq:connect receiver "tcp://localhost:5557")
      ;; Connect to weather server
      (zmq:with-socket (subscriber context zmq:sub)
        (zmq:connect subscriber "tcp://localhost:5556")
        (zmq:setsockopt subscriber zmq:subscribe "10001 ")

        ;; Initialize poll set
        (zmq:with-polls ((items . ((receiver   . zmq:pollin)
                                   (subscriber . zmq:pollin))))
          ;; Process messages from both sockets
          (loop
            (let ((revents (zmq:poll items)))
              (when (= (first revents) zmq:pollin)
                (let ((message (make-instance 'zmq:msg)))
                  (zmq:recv receiver message)
                  ;; Process task
                  (dump-message message)
                  (finish-output)))

              (when (= (second revents) zmq:pollin)
                (let ((message (make-instance 'zmq:msg)))
                  (zmq:recv subscriber message)
                  ;; Process weather update
                  (dump-message message)
                  (finish-output)))))))))

  (cleanup))
