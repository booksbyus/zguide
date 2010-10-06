;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Reading from multiple sockets in Common Lisp
;;;  This version uses a simple recv loop
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.msreader
  (:nicknames #:msreader)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.msreader)

(defun main ()
  ;; Prepare our context and socket
  (zmq:with-context (context 1)
    ;; Connect to task ventilator
    (zmq:with-socket (receiver context zmq:pull)
      (zmq:connect receiver "tcp://localhost:5557")
      ;; Connect to weather server
      (zmq:with-socket (subscriber context zmq:sub)
        (zmq:connect subscriber "tcp://localhost:5556")
        (zmq:setsockopt subscriber zmq:subscribe "10001 ")

        ;; Process messages from both sockets
        ;; We prioritize traffic from the task ventilator
        (loop
          (handler-case
              (loop
                (let ((task (make-instance 'zmq:msg)))
                  (zmq:recv receiver task zmq:noblock)
                  ;; process task
                  (dump-message task)
                  (finish-output)))

            (zmq:error-again () nil))

          ;; Process any waiting weather updates
          (handler-case
              (loop
                (let ((update (make-instance 'zmq:msg)))
                  (zmq:recv subscriber update zmq:noblock)
                  ;; process weather update
                  (dump-message update)
                  (finish-output)))

            (zmq:error-again () nil))

          ;; No activity, so sleep for 1 msec
          (isys:usleep 1000)))))

  (cleanup))
