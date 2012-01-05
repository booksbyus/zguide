;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Multithreaded Hello World server in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.mtserver
  (:nicknames #:mtserver)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.mtserver)

(defun worker-routine (context)
  ;; Socket to talk to dispatcher
  (zmq:with-socket (receiver context zmq:rep)
    (zmq:connect receiver "inproc://workers")

    (loop
      (let ((request (make-instance 'zmq:msg)))
        (zmq:recv receiver request)
        (message "Received request: [~A]~%" (zmq:msg-data-as-string request))

        ;; Do some 'work'
        (sleep 1)

        ;; Send reply back to client
        (let ((reply (make-instance 'zmq:msg :data "World")))
          (zmq:send receiver reply))))))

(defun main ()
  ;; Prepare our context and socket
  (zmq:with-context (context 1)
    ;; Socket to talk to clients
    (zmq:with-socket (clients context zmq:router)
      (zmq:bind clients "tcp://*:5555")
      ;; Socket to talk to workers
      (zmq:with-socket (workers context zmq:dealer)
        (zmq:bind workers "inproc://workers")

        ;; Launch pool of worker threads
        (dotimes (i 5)
          (bt:make-thread (lambda () (worker-routine context))
                          :name (format nil "worker-~D" i)))

        ;; Connect work threads to client threads via a queue
        (zmq:device zmq:queue clients workers))))

  (cleanup))
