;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Task worker in Common Lisp
;;;  Connects PULL socket to tcp://localhost:5557
;;;  Collects workloads from ventilator via that socket
;;;  Connects PUSH socket to tcp://localhost:5558
;;;  Sends results to sink via that socket
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.taskwork
  (:nicknames #:taskwork)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.taskwork)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket to receive messages on
    (zmq:with-socket (receiver context zmq:pull)
      (zmq:connect receiver "tcp://localhost:5557")

      ;; Socket to send messages to
      (zmq:with-socket (sender context zmq:push)
        (zmq:connect sender "tcp://localhost:5558")

        ;; Process tasks forever
        (loop
          (let ((pull-msg (make-instance 'zmq:msg)))
            (zmq:recv receiver pull-msg)

            (let* ((string (zmq:msg-data-as-string pull-msg))
                   (delay (* (parse-integer string) 1000)))
              ;; Simple progress indicator for the viewer
              (message "~A." string)

              ;; Do the work
              (isys:usleep delay)

              ;; Send results to sink
              (let ((push-msg (make-instance 'zmq:msg :data "")))
                (zmq:send sender push-msg))))))))

  (cleanup))
