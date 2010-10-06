;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Task worker - design 2 in Common Lisp
;;;  Connects PULL socket to tcp://localhost:5557
;;;  Collects workloads from ventilator via that socket
;;;  Connects PUSH socket to tcp://localhost:5558
;;;  Sends results to sink via that socket
;;;  Adds pub-sub flow to receive and respond to kill signal
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.taskwork2
  (:nicknames #:taskwork2)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.taskwork2)

(defun main ()
  (zmq:with-context (context 1)
    ;; Socket to receive messages on
    (zmq:with-socket (receiver context zmq:pull)
      (zmq:connect receiver "tcp://localhost:5557")

      ;; Socket to send messages to
      (zmq:with-socket (sender context zmq:push)
        (zmq:connect sender "tcp://localhost:5558")

        ;; Socket for control input
        (zmq:with-socket (controller context zmq:sub)
          (zmq:connect controller "tcp://localhost:5559")
          (zmq:setsockopt controller zmq:subscribe "")

          ;; Process messages from receiver and controller
          (zmq:with-polls ((items . ((receiver   . zmq:pollin)
                                     (controller . zmq:pollin))))
            (loop
              (let ((revents (zmq:poll items)))
                (when (= (first revents) zmq:pollin)
                  (let ((pull-msg (make-instance 'zmq:msg)))
                    (zmq:recv receiver pull-msg)

                    ;; Process task
                    (let* ((string (zmq:msg-data-as-string pull-msg))
                           (delay (* (parse-integer string) 1000)))
                      ;; Simple progress indicator for the viewer
                      (message "~A." string)

                      ;; Do the work
                      (isys:usleep delay)

                      ;; Send results to sink
                      (let ((push-msg (make-instance 'zmq:msg :data "")))
                        (zmq:send sender push-msg)))))

                (when (= (second revents) zmq:pollin)
                  ;; Any waiting controller command acts as 'KILL'
                  (return)))))))))

  (cleanup))
