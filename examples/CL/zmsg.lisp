;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Multipart message class for example applications in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.zmsg
  (:nicknames #:zmsg)
  (:use #:cl)
  (:shadow #:recv #:send #:push #:pop)
  (:export
   #:message
   #:make-message
   #:recv
   #:send
   #:parts
   #:body
   #:set-body
   #:format-body
   #:push
   #:pop
   #:address
   #:wrap
   #:unwrap
   #:dump
   #:test))

(in-package :zguide.zmsg)

(defparameter +hex-char+ "0123456789ABCDEF")

(defparameter +hex-to-bin+
  #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
     0  1  2  3  4  5  6  7  8  9 -1 -1 -1 -1 -1 -1
    -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))

(defun uuid-encode (data)
  "Formats 17-byte UUID as 33-char string starting with '@'."
  (assert (and (= (array-rank data) 1)
	       (= (array-total-size data) 17)
               (= 0 (aref data 0))))
  (let ((uuid (make-array 32 :element-type 'character)))
    (dotimes (i 16)
      (setf (char uuid (* i 2))      (char +hex-char+ (ash (aref data (1+ i)) -4)))
      (setf (char uuid (1+ (* i 2))) (char +hex-char+ (logand (aref data (1+ i)) 15))))
    (format nil "@~A" uuid)))

(defun uuid-decode (uuid)
  "Decodes 33-char string starting with '@' to 17-byte UUID."
  (assert (and (= 33 (length uuid))
               (char= #\@ (char uuid 0))))
  (let ((data (make-array 17 :element-type '(unsigned-byte 8))))
    (setf (aref data 0) 0)
    (dotimes (i 16)
      (setf (aref data (1+ i))
            (+ (ash
                (aref +hex-to-bin+
                      (logand (char-code
                               (char uuid (1+ (* 2 i))))
                              127))
                4)
               (aref +hex-to-bin+
                     (logand (char-code
                              (char uuid (+ (* 2 i) 2)))
                             127)))))
    data))

;; * We don't care about zero-copy performance, so messages hold copies of data.
;; * Receiving a message always calls the constructor so we don't need extra constructors.
;; * Sending a message always calls the destructor, and sets the message reference to null.
;; * Message parts (addresses and data) are always printable strings.

(defstruct (message
             (:constructor %make-message)
             (:copier nil))
  (parts       nil :type (or null list))
  (parts-count 0   :type fixnum))

(defun make-message ()
  "Creates empty message."
  (%make-message))

(defun recv (socket)
  "Receives message from socket.

Creates a new message and returns it. Blocks on recv if socket is not ready
for input."
  (loop :with parts = (list)
        :for parts-count :from 1
        :do (let ((msg (make-instance 'zmq:msg)))
              (zmq:recv socket msg)
              ;; We handle 0MQ UUIDs as printable strings
              (let ((data (zmq:msg-data-as-array msg)))
                (if (and (= 17 (zmq:msg-size msg))
                         (= (aref data 0) 0))
                    ;; Store message part as string uuid
                    (let ((uuid (uuid-encode data)))
                      (cl:push (cons (length uuid) uuid) parts))
                    ;; Store this message part
                    (cl:push (cons (zmq:msg-size msg) (zmq:msg-data-as-string msg)) parts))))
        :until (zerop (zmq:getsockopt socket zmq:rcvmore)) ; last message part
        :finally (return (%make-message :parts (nreverse parts) :parts-count parts-count))))

(defun send (message socket)
  "Sends message to socket."
  ;; Unmangle 0MQ identities for writing to the socket
  (loop :for (size . text) :in (message-parts message)
        :for msg = (if (and (= 33 size)
                            (char= #\@ (char text 0)))
                       (make-instance 'zmq:msg :data (uuid-decode text) :size size)
                       (make-instance 'zmq:msg :data text))
        :do (if (zerop (decf (message-parts-count message)))
                (zmq:send socket msg)
                (zmq:send socket msg zmq:sndmore))))

(defun parts (message)
  "Reports size of message."
  (message-parts-count message))

(defun body (message)
  "Returns message body, if any.

Caller should not modify the provided data."
  (and (message-parts message)
       (cdr (nth (1- (message-parts-count message)) (message-parts message)))))

(defun set-body (message body)
  "Sets message body as copy of provided string.

If message is empty, creates a new message body."
  (let ((part (cons (length body) body)))
    (if (null (message-parts message))
        (progn
          (cl:push part (message-parts message))
          (incf (message-parts-count message)))
        (setf (nth (1- (message-parts-count message)) (message-parts message)) part)))
  (values))

(defun format-body (message fmt &rest args)
  "Sets message body using format.

If message is empty, creates a new message body."
  (set-body message (apply #'format nil fmt args)))

(defun push (message part)
  "Pushes message part to front of message parts."
  (incf (message-parts-count message))
  (cl:push (cons (length part) part) (message-parts message))
  (values))

(defun pop (message)
  "Pops message part off front of message parts."
  (decf (message-parts-count message))
  (cdr (cl:pop (message-parts message))))

(defun address (message)
  "Returns pointer to outer message address, if any.

Caller should not modify the provided data."
  (cdr (car (message-parts message))))

(defun wrap (message address &optional delim)
  "Wraps message in new address envelope.

If delim is not NIL, creates two-part envelope."
  (when delim
    ;; Push optional delimiter and then address
    (push message delim))
  (push message address))

(defun unwrap (message)
  "Unwraps outer message envelope and returns address.

Discards empty message part after address, if any."
  (prog1 (pop message)
    (when (zerop (length (address message)))
      (pop message))))

(defun text-part-p (part)
  (loop :for char :across part
        :for code = (char-code char)
        :never (or (> 32 code) (< 127 code))))

(defun dump (message)
  "Dumps message for debugging and tracing."
  ;; Dump the message as text or binary
  (let ((*standard-output* *error-output*))
    (loop :for (size . data) in (message-parts message)
          :do (format t "[~3,'0D] " size)
              (if (text-part-p data)
                  (write-string data)
                  (loop :for x :across data
                        :do (format t "~2,'0X" (char-code x))))
              (terpri))
    (finish-output)))

(defun test (&optional verbosep)
  "Runs self test of class."
  (format t " * zmsg: ")

  ;; Prepare our context and sockets
  (zmq:with-context (context 1)
    (zmq:with-socket (output context zmq:dealer)
      (zmq:bind output "ipc://zmsg_selftest.ipc")

      (zmq:with-socket (input context zmq:router)
        (zmq:connect input "ipc://zmsg_selftest.ipc")

        ;; Test send and receive of single-part message
        (let ((message (make-message)))
          (set-body message "Hello")
          (assert (string= (body message) "Hello"))
          (send message output)

          (let ((message (recv input)))
            (assert (= (parts message) 2))
            (when verbosep
              (dump message))
            (assert (string= (body message) "Hello"))))

        ;; Test send and receive of multi-part message
        (let ((message (make-message)))
          (set-body message "Hello")
          (wrap message "address1" "")
          (wrap message "address2")
          (assert (= (parts message) 4))
          (send message output)

          (let ((message (recv input)))
            (when verbosep
              (dump message))
            (assert (= (parts message) 5))
            (assert (= 33 (length (address message))))
            (unwrap message)
            (assert (string= (address message) "address2"))
            (format-body message "~C~A" #\W "orld")
            (send message output))

          (let ((message (recv input)))
            (unwrap message)
            (assert (= (parts message) 4))
            (string= (body message) "World")
            (let ((part (unwrap message)))
              (assert (string= part "address2")))

            ;; Pull off address 1, check that empty part was dropped
            (let ((part (unwrap message)))
              (assert (string= part "address1"))
              (assert (= 1 (parts message))))

            ;; Check that message body was correctly modified
            (let ((part (pop message)))
              (assert (string= part "World"))
              (assert (zerop (parts message)))))

          (format t "OK~%"))))))

;; [033] @02E39A16025D42E68BEA84F25D6A9A0A
;; [005] Hello
;; [033] @02E39A16025D42E68BEA84F25D6A9A0A
;; [008] address2
;; [008] address1
;; [000]
;; [005] Hello
;;  * zmsg: OK
