;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Least-recently used (LRU) queue device in Common Lisp
;;;  Clients and workers are shown here in-process
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.lruqueue
  (:nicknames #:lruqueue)
  (:use #:cl #:zhelpers)
  (:shadow #:message)
  (:export #:main))

(in-package :zguide.lruqueue)

(defun message (fmt &rest args)
  (let ((new-fmt (format nil "[~A] ~A"
                         (bt:thread-name (bt:current-thread)) fmt)))
    (apply #'zhelpers:message new-fmt args)))

(defparameter *number-clients* 10)
(defparameter *number-workers* 3)

;; Basic request-reply client using REQ socket
(defun client-thread (context)
  (zmq:with-socket (client context zmq:req)
    (set-socket-id client)              ; Makes tracing easier
    (zmq:connect client "ipc://frontend.ipc")

    ;; Send request, get reply
    (send-text client "HELLO")
    (let ((reply (recv-text client)))
      (message "Client: ~A~%" reply))))

;; Worker using REQ socket to do LRU routing
(defun worker-thread (context)
  (zmq:with-socket (worker context zmq:req)
    (set-socket-id worker)              ; Makes tracing easier
    (zmq:connect worker "ipc://backend.ipc")

    ;; Tell broker we're ready for work
    (send-text worker "READY")

    ;; Ignore errors and exit when the context gets terminated
    (ignore-errors
      (loop
        ;; Read and save all frames until we get an empty frame
        ;; In this example there is only 1 but it could be more
       (let ((address (recv-text worker)))
         (recv-text worker)             ; empty

         ;; Get request, send reply
         (let ((request (recv-text worker)))
           (message "Worker: ~A~%" request)

           (send-more-text worker address)
           (send-more-text worker "")
           (send-text worker "OK")))))))

(defun main ()
  ;; Prepare our context and sockets
  (zmq:with-context (context 1)
    (zmq:with-socket (frontend context zmq:router)
      (zmq:with-socket (backend context zmq:router)
        (zmq:bind frontend "ipc://frontend.ipc")
        (zmq:bind backend "ipc://backend.ipc")

        (dotimes (i *number-clients*)
          (bt:make-thread (lambda () (client-thread context))
                          :name (format nil "client-thread-~D" i)))
        (dotimes (i *number-workers*)
          (bt:make-thread (lambda () (worker-thread context))
                          :name (format nil "worker-thread-~D" i)))

        ;; Logic of LRU loop
        ;; - Poll backend always, frontend only if 1+ worker ready
        ;; - If worker replies, queue worker as ready and forward reply
        ;;   to client if necessary
        ;; - If client requests, pop next worker and send request to it

        ;; Queue of available workers
        (let ((number-clients *number-clients*)
              (available-workers 0)
              (worker-queue (make-queue)))
          (loop
            ;; Initialize poll set
           (zmq:with-polls
               ((items2 .
                        ;; Always poll for worker activity on backend
                        ((backend  . zmq:pollin)
                         (frontend . zmq:pollin)))
                (items1 .
                        ;; Poll front-end only if we have available workers
                        ((backend  . zmq:pollin))))
             (let ((revents
                    (if (zerop available-workers)
                        (zmq:poll items1)
                        (zmq:poll items2))))

               ;; Handle worker activity on backend
               (when (= (first revents) zmq:pollin)
                 ;; Queue worker address for LRU routing
                 (let ((worker-addr (recv-text backend)))
                   (assert (< available-workers *number-workers*))
                   (enqueue worker-queue worker-addr)
                   (incf available-workers))

                 ;; Second frame is empty
                 (recv-text backend)    ; empty

                 ;; Third frame is READY or else a client reply address
                 (let ((client-addr (recv-text backend)))
                   (when (string/= client-addr "READY")
                     (recv-text backend) ; empty

                     (let ((reply (recv-text backend)))
                       (send-more-text frontend client-addr)
                       (send-more-text frontend "")
                       (send-text frontend reply))

                     (when (zerop (decf number-clients))
                       (return)))))

               (when (and (cdr revents)
                          (= (second revents) zmq:pollin))
                 ;; Now get next client request, route to LRU worker
                 ;; Client request is [address][empty][request]
                 (let ((client-addr (recv-text frontend)))
                   (recv-text frontend) ; empty

                   (let ((request (recv-text frontend)))
                     (send-more-text backend (dequeue worker-queue))
                     (send-more-text backend "")
                     (send-more-text backend client-addr)
                     (send-more-text backend "")
                     (send-text backend request))

                   (decf available-workers)))))))))
    (sleep 2))

  (cleanup))
