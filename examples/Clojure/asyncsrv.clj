(ns asyncsrv
  (:require [zeromq [zmq :as zmq]
                    [device :as zmqd]])
  (:import  [java.util Random]))

;;
;; Asynchronous client-to-server (DEALER to ROUTER)
;;
;; While this example runs in a single process, that is just to make
;; it easier to start and stop the example. Each task has its own
;; context and conceptually acts as a separate process.

;; Accept a request and reply with the same text a random number of
;; times, with random delays between replies.

(defrecord ServerWorker [ctx]
  Runnable
  (run [this]
    (let [srandom (Random. (System/currentTimeMillis))
          worker (doto (zmq/socket ctx :dealer)
                   (zmq/connect "inproc://backend"))]
      (while true
        ;; The DEALER socket gives us the address envelope and message
        (let [[identity content] (zmq/receive-all worker)]
          ;; Send 0..4 replies back
          (dotimes [i (.nextInt srandom 5)]
            ;; Sleep for some fraction of a second
            (Thread/sleep (inc (.nextInt srandom 1000)))
            (zmq/send worker identity zmq/send-more)
            (zmq/send worker content)))))))


;; This is our client task
;; It connects to the server, and then sends a request once per second
;; It collects responses as they arrive, and it prints them out. We will
;; run several client tasks in parallel, each with a different random ID.

(defrecord ClientTask []
  Runnable
  (run [this]
    (let [ctx (zmq/context 1)
          ;; Set random identity to make tracing easier
          random (Random.)
          identity (format "%04X-%04X"
                           (.nextInt random 0x10000)
                           (.nextInt random 0x10000))
          client (doto (zmq/socket ctx :dealer)
                   (zmq/set-identity (.getBytes identity))
                   (zmq/connect "tcp://localhost:5570"))
          poller (doto (zmq/poller ctx 1)
                   (zmq/register client :pollin))
          request-nbr (atom 0)]
      (while true
        ;; Tick once per second, pulling in arriving messages
        (dotimes [_ 100]
          (zmq/poll poller 10)
          (when (zmq/check-poller poller 0 :pollin)
            (println (format "%s: %s"
                             identity
                             (zmq/receive-str client)))))
        (zmq/send-str client (format "request: %d" (swap! request-nbr inc)))))))

(defrecord ServerTask []
  Runnable
  (run [this]
    (let [ctx (zmq/context 1)
          ;; Frontend socket talks to clients over TCP
          frontend (doto (zmq/socket ctx :router)
                     (zmq/bind "tcp://*:5570"))
          ;; Backend socket talks to workers over inproc
          backend (doto (zmq/socket ctx :dealer)
                    (zmq/bind "inproc://backend"))]

      ;; Launch pool of worker threads, precise number is not critical
      (dotimes [_ 5]
        (-> (->ServerWorker ctx) Thread. .start))

      ;; Connect backend to frontend via a proxy
      (zmqd/proxy ctx frontend backend)

      (zmq/destroy ctx))))

;; The main thread simply starts several clients and a server, and then
;; waits for the server to finish
(defn -main []
  (dotimes [_ 3]
    (-> (ClientTask.) Thread. .start))
  (-> (ServerTask.) Thread. .start)
  (Thread/sleep 5000) ;; Run for 5 seconds then quit
  (System/exit 0))
