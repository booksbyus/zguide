;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Multithreaded relay in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.mtrelay
  (:nicknames #:mtrelay)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.mtrelay)

(defun step1 (context)
  ;; Signal downstream to step 2
  (zmq:with-socket (sender context zmq:pair)
    (zmq:connect sender "inproc://step2")

    (let ((msg (make-instance 'zmq:msg :data "")))
      (zmq:send sender msg))))

(defun step2 (context)
  ;; Bind to inproc: endpoint, then start upstream thread
  (zmq:with-socket (receiver context zmq:pair)
    (zmq:bind receiver "inproc://step2")
    (bt:make-thread (lambda () (step1 context)))

    ;; Wait for signal
    (let ((msg (make-instance 'zmq:msg)))
      (zmq:recv receiver msg))

    ;; Signal downstream to step 3
    (zmq:with-socket (sender context zmq:pair)
      (zmq:connect sender "inproc://step3")

      (let ((msg (make-instance 'zmq:msg :data "")))
        (zmq:send sender msg)))))

(defun main ()
  (zmq:with-context (context 1)
    ;; Bind to inproc: endpoint, then start upstream thread
    (zmq:with-socket (receiver context zmq:pair)
      (zmq:bind receiver "inproc://step3")
      (bt:make-thread (lambda () (step2 context)))

      ;; Wait for signal
      (let ((msg (make-instance 'zmq:msg)))
        (zmq:recv receiver msg)))

    (message "Test successful!~%"))

  (cleanup))
