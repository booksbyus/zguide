;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Weather update client in Common Lisp
;;;  Connects SUB socket to tcp://localhost:5556
;;;  Collects weather updates and finds avg temp in zipcode
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.wuclient
  (:nicknames #:wuclient)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.wuclient)

(defun main ()
  (zmq:with-context (context 1)
    (message "Collecting updates from weather server...~%")

    ;; Socket to talk to server
    (zmq:with-socket (subscriber context zmq:sub)
      (zmq:connect subscriber "tcp://localhost:5556")

      ;; Subscribe to zipcode, default is NYC, 10001
      (let ((filter (or (first (cmd-args)) "10001 ")))
        (zmq:setsockopt subscriber zmq:subscribe filter)

        ;; Process 100 updates
        (let ((number-updates 100)
              (total-temp 0.0))

          (loop :repeat number-updates :do
            (let ((update (make-instance 'zmq:msg)))
              (zmq:recv subscriber update)

              (destructuring-bind (zipcode_ temperature relhumidity_)
                  (split-sequence:split-sequence #\Space (zmq:msg-data-as-string update))

                (declare (ignore zipcode_ relhumidity_))
                (incf total-temp (parse-integer temperature)))))

          (message "Average temperature for zipcode ~A was ~FF~%"
                   filter (/ total-temp number-updates))))))

  (cleanup))
