(ns asyncsrv
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import (org.zeromq ZMQ$Poller)
           (java.util Random)))

;;
;; Asynchronous client-to-server (DEALER to ROUTER)
;;
;; While this example runs in a single process, that is just to make
;; it easier to start and stop the example. Each task has its own
;; context and conceptually acts as a separate process.
;;
;; Isaiah Peng <issaria@gmail.com>

;; Accept a request and reply with the same text a random number of
;; times, with random delays between replies.

(defrecord ServerWorker []
  Runnable
  (run [this]
    (let [ctx (mq/context 1)
          srandom (Random. (System/currentTimeMillis))
          worker (mq/socket ctx mq/dealer)]
      (mq/connect worker "inproc://backend")
      ;; The DEALER socket gives us the address envelope and message
      (let [msg (-> worker mq/recv-all last String.)]
        (dotimes [i (.nextInt srandom 5)]
          (Thread/sleep (* 1000 (.nextInt srandom 0)))
          (mq/send worker msg))))))

;; ---------------------------------------------------------------------
;; This is our client task
;; It connects to the server, and then sends a request once per second
;; It collects responses as they arrive, and it prints them out. We will
;; run several client tasks in parallel, each with a different random ID.

(defrecord ClientTask [n]
  Runnable
  (run [this]
    (let [ctx (mq/context 1)
          client (mq/socket ctx mq/dealer)
          poller (.poller ctx 1)
          request-nbr (atom 0)]
      (mq/set-id client n)
      (mq/connect client "tcp://localhost:5570")
      (.register poller client ZMQ$Poller/POLLIN)
      (while true
        (dotimes [i 100]
          (.poll poller 10000)
          (if (.pollin poller 0)
            (println (format "%s: %s" (-> client .getIdentity String.) (-> client (mq/recv-all 0) last))))
          (swap! request-nbr inc)
          (mq/send client (format "request: %d" @request-nbr)))))))

(defrecord ServerTask []
  Runnable
  (run [this]
    (let [ctx (mq/context 1)
          frontend (mq/socket ctx mq/router)
          backend (mq/socket ctx mq/dealer)]
      ;; Frontend socket talks to clients over tcp
      (mq/bind frontend "tcp://*:5570")
      ;; Backend socket talks to workers over inproc
      (mq/bind backend "inproc://backend")
      ;; Launch pool of worker threads, precise number is not critical
      (dotimes [i 5]
        (-> (ServerWorker.) Thread. .start))
      ;;  Connect backend to frontend via a queue device
      ;;  We could do this:
      ;;  zmq_device (ZMQ_QUEUE, frontend, backend);
      ;;  But doing it ourselves means we can debug this more easily
      ;; 
      ;;  Switch messages between frontend and backend
      (let [poller (.poller ctx 2)]
        (.register poller frontend ZMQ$Poller/POLLIN)
        (.register poller backend ZMQ$Poller/POLLIN)
        (while true
          (.poll poller)
          (if (.pollin poller 0)
            (let [msg (-> frontend (mq/recv-all 0) last String.)]
              (println (format "Request from client: %s" msg))
              (mq/send backend msg)))
          (if (.pollin poller 1)
            (let [msg (-> backend (mq/recv-all 0) last String.)]
              (println (format "Reply from worker: %s" msg))
              (mq/send frontend msg))))))))

(defn -main []
  (dotimes [i 3]
    (-> i ClientTask. Thread. .start))
  (-> (ServerTask.) Thread. .start))
