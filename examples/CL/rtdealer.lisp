;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Custom routing Router to Dealer in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

;;;  We have two workers, here we copy the code, normally these would run on
;;;  different boxes...

(defpackage #:zguide.rtdealer
  (:nicknames #:rtdealer)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.rtdealer)

(defun worker-a (context)
  (zmq:with-socket (worker context zmq:dealer)
    (zmq:setsockopt worker zmq:identity "A")
    (zmq:connect worker "ipc://routing.ipc")

    (let ((total 0))
      (loop
        ;; We receive one part, with the workload
        (let ((request (recv-text worker)))
          (when (string= request "END")
            (message "A received: ~D~%" total)
            (return))
          (incf total))))))

(defun worker-b (context)
  (zmq:with-socket (worker context zmq:dealer)
    (zmq:setsockopt worker zmq:identity "B")
    (zmq:connect worker "ipc://routing.ipc")

    (let ((total 0))
      (loop
        ;; We receive one part, with the workload
        (let ((request (recv-text worker)))
          (when (string= request "END")
            (message "B received: ~D~%" total)
            (return))
          (incf total))))))

(defun main ()
  (zmq:with-context (context 1)
    (zmq:with-socket (client context zmq:router)
      (zmq:bind client "ipc://routing.ipc")

      (bt:make-thread (lambda () (worker-a context))
                      :name "worker-a")
      (bt:make-thread (lambda () (worker-b context))
                      :name "worker-b")

      ;; Wait for threads to stabilize
      (sleep 1)

      ;; Send 10 tasks scattered to A twice as often as B
      (loop :repeat 10 :do
        ;; Send two message parts, first the address...
        (if (> (1- (within 3)) 0)
            (send-more-text client "A")
            (send-more-text client "B"))

        ;; And then the workload
        (send-text client "This is the workload"))

      (send-more-text client "A")
      (send-text client "END")
      ;; we can get messy output when two threads concurrently print results
      ;; so Let worker-a to print results first
      (sleep 0.1)

      (send-more-text client "B")
      (send-text client "END")

      ;; Give 0MQ/2.0.x time to flush output
      (sleep 1)))

  (cleanup))
