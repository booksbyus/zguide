;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Helpers for example applications
;;;
;;; 'with-stopwatch' macro is taken from 'cl-zmq' by Vitaly Mayatskikh
;;; <v.mayatskih@gmail.com>
;;;
;;; A simple, but efficient, queue implementation was originally written by
;;; Peter Keller (psilord@cs.wisc.edu) and is released under the same license
;;; as IOLib.
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.zhelpers
  (:nicknames #:zhelpers)
  (:use #:cl)
  (:export
   #:cmd-args
   #:message
   #:cleanup
   #:within
   #:version
   #:set-socket-id
   #:recv-text
   #:send-text
   #:send-more-text
   #:dump-message
   #:dump-socket
   #:with-stopwatch
   #:make-queue
   #:enqueue
   #:dequeue
   #:empty-queue-p))

(in-package :zguide.zhelpers)

(defun cmd-args ()
  "Return command line arguments."
  (rest
   (or
    #+sbcl sb-ext:*posix-argv*
    #+ccl ccl:*command-line-argument-list*
    #+clisp ext:*args*
    #+lispworks system:*line-arguments-list*
    #+ecl (ext:command-args)
    nil)))

(defun message (fmt &rest args)
  (apply #'format t fmt args)
  (finish-output))

(defun cleanup ()
  "Cleanup and exit."
  (tg:gc)
  #+sbcl (sb-ext:quit)
  #+ccl (ccl:quit)
  #+clisp (ext:quit)
  #+lispworks (lispworks:quit)
  #+ecl (ext:quit))

(defun within (num)
  "Provide random number from 1..num."
  (1+ (random num)))

(defun version ()
  "Report 0MQ version number."
  (message "Current 0MQ version is ~A~%" (zmq:version)))

(defun set-socket-id (socket)
  "Set simple random printable identity on socket."
  (zmq:setsockopt socket zmq:identity
                  (format nil "~4,'0X-~4,'0X"
                          (within #x10000) (within #x10000))))

(defun recv-text (socket)
  "Receive 0MQ string from socket and convert into text"
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv socket msg)
    (zmq:msg-data-as-string msg)))

(defun send-text (socket text)
  "Convert text to 0MQ string and send to socket"
  (let ((msg (make-instance 'zmq:msg :data text)))
    (zmq:send socket msg)))

(defun send-more-text (socket text)
  "Sends text as 0MQ string, as multipart non-terminal"
  (let ((msg (make-instance 'zmq:msg :data text)))
    (zmq:send socket msg zmq:sndmore)))

(defun text-message-p (msg)
  (let ((data (zmq:msg-data-as-is msg)))
    (loop :for i :from 0 :to (1- (zmq:msg-size msg))
          :with char = (cffi:mem-ref data :char i) :do
      (when (or (< char 32) (> char 127))
        (return-from text-message-p nil)))
    (values t)))

(defun dump-binary-message (msg)
  (let ((data (zmq:msg-data-as-array msg)))
    (loop :for x :across data :do
      (format t "~2,'0X" x))))

(defun dump-message (msg)
  (format t "[~3,'0D] " (zmq:msg-size msg))
  (if (text-message-p msg)
      (write-string (zmq:msg-data-as-string msg))
      (dump-binary-message msg))
  (terpri))

(defun dump-socket (socket)
  "Receive all message parts from socket, print neatly."
  (format t "----------------------------------------~%")

  (loop
    ;; Process all parts of the message
   (let ((message (make-instance 'zmq:msg)))
     (zmq:recv socket message)

     ;; Dump the message as text or binary
     (dump-message message)
     (finish-output)

     ;; Multipart detection
     (when (zerop (zmq:getsockopt socket zmq:rcvmore))
       (return)))))

(defmacro with-stopwatch (&body body)
  (let ((sec0 (gensym))
        (sec1 (gensym))
        (usec0 (gensym))
        (usec1 (gensym)))
    `(multiple-value-bind (,sec0 ,usec0)
         (isys:gettimeofday)
       (unwind-protect
            (progn ,@body))
       (multiple-value-bind (,sec1 ,usec1)
           (isys:gettimeofday)
         (+ (* 1e6 (- ,sec1 ,sec0))
            ,usec1 (- ,usec0))))))

(defun make-queue ()
  (cons nil nil))

(defun enqueue (q obj)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun empty-queue-p (q)
  (null (car q)))
