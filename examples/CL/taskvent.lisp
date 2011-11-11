;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Task ventilator in Common Lisp
;;;  Binds PUSH socket to tcp://localhost:5557
;;;  Sends batch of tasks to workers via that socket
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.taskvent
  (:nicknames #:taskvent)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.taskvent)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket to send messages on
    (zmq:with-socket (sender context zmq:push)
      (zmq:bind sender "tcp://*:5557")

      (message "Press Enter when the workers are ready: ")
      (read-char)
      (message "Sending tasks to workers...~%")

      ;; The first message is "0" and signals start of batch
      (let ((msg (make-instance 'zmq:msg :data "0")))
        (zmq:send sender msg))

      ;; Send 100 tasks
      (let ((total-msec 0))
        (loop :repeat 100 :do
          ;; Random workload from 1 to 100 msecs
          (let ((workload (within 100)))
            (incf total-msec workload)
            (let ((msg (make-instance 'zmq:msg
                                      :data (format nil "~D" workload))))
              (zmq:send sender msg))))

        (message "Total expected cost: ~D msec~%" total-msec)
        ;; Give 0MQ time to deliver
        (sleep 1))))

  (cleanup))
