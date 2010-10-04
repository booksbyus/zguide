;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Weather update server in Common Lisp
;;;  Binds PUB socket to tcp://*:5556
;;;  Publishes random weather updates
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.wuserver
  (:nicknames #:wuserver)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.wuserver)

(defun main ()
  ;; Prepare our context and socket
  (zmq:with-context (context 1)
    (zmq:with-socket (publisher context zmq:pub)
      (zmq:bind publisher "tcp://*:5556")
      (zmq:bind publisher "ipc://weather.ipc")

      (loop
        ;; Get values that will fool the boss
        (let ((zipcode (within 100000))
              (temperature (- (within 215) 80))
              (relhumidity (+ (within 50) 10)))

          ;; Send message to all subscribers
          (let ((message
                 (make-instance 'zmq:msg
                                :data (format nil "~5,'0D ~D ~D"
                                              zipcode
                                              temperature
                                              relhumidity))))

            ;; Send message to all subscribers
            (zmq:send publisher message))))))

  (cleanup))
