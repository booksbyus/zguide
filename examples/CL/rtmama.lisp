;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Custom routing Router to Mama (ROUTER to REQ) in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rtmama
  (:nicknames #:rtmama)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.rtmama)

(defparameter *number-workers* 10)

(defun worker-thread (context)
  (zmq:with-socket (worker context zmq:req)
    ;; We use a string identity for ease here
    (set-socket-id worker)
    (zmq:connect worker "ipc://routing.ipc")

    (let ((total 0))
      (loop
        ;; Tell the router we're ready for work
        (send-text worker "ready")

        ;; Get workload from router, until finished
        (let ((workload (recv-text worker)))
          (when (string= workload "END")
            (message "Processed: ~D tasks~%" total)
            (return))
          (incf total))

        ;; Do some random work
        (isys:usleep (within 100000))))))

(defun main ()
  (zmq:with-context (context 1)
    (zmq:with-socket (client context zmq:router)
      (zmq:bind client "ipc://routing.ipc")

      (dotimes (i *number-workers*)
        (bt:make-thread (lambda () (worker-thread context))
                        :name (format nil "worker-thread-~D" i)))

      (loop :repeat (* 10 *number-workers*) :do
        ;; LRU worker is next waiting in queue
        (let ((address (recv-text client)))
          (recv-text client) ; empty
          (recv-text client) ; ready

          (send-more-text client address)
          (send-more-text client "")
          (send-text client "This is the workload")))

      ;; Now ask mamas to shut down and report their results
      (loop :repeat *number-workers* :do
        ;; LRU worker is next waiting in queue
        (let ((address (recv-text client)))
          (recv-text client) ; empty
          (recv-text client) ; ready

          (send-more-text client address)
          (send-more-text client "")
          (send-text client "END")))

      ;; Give 0MQ/2.0.x time to flush output
      (sleep 1)))

  (cleanup))
