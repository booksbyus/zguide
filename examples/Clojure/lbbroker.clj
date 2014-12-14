(ns lbbroker
  (:require [zeromq.zmq :as zmq]))

;;
;; Least-recently used (LRU) queue device
;; Clients and workers are shown here in-process
;;
;; While this example runs in a single process, that is just to make
;; it easier to start and stop the example. Each thread has its own
;; context and conceptually acts as a separate process.
;;

(defn client [n]
  (fn []
    (let [ctx (zmq/context 1)
          client (zmq/socket ctx :req)]
      (zmq/set-identity client (-> n str .getBytes))
      (zmq/connect client "ipc://frontend.ipc")
      (zmq/send-str client "HELLO")
      (println "Client:" (zmq/receive-str client))
      (.close client)
      (.term ctx))))

(defn worker [n]
  (fn []
    (let [ctx (zmq/context 1)
          worker (zmq/socket ctx :req)]
      (zmq/set-identity worker (-> n str .getBytes))
      (zmq/connect worker "ipc://backend.ipc")
      (zmq/send-str worker "READY")
      (while true
        (let [identity (zmq/receive-str worker)
              _ (zmq/receive worker)
              request (zmq/receive-str worker)]
          (println "Worker:" request)
          (zmq/send-str worker identity zmq/send-more)
          (zmq/send-str worker "" zmq/send-more)
          (zmq/send-str worker "OK")))
      (.close worker)
      (.term ctx))))

(def worker-nbr 3)
(def client-nbr 10)

(defn -main []
  (let [ctx (zmq/context 1)
        frontend (zmq/socket ctx :router)
        backend (zmq/socket ctx :router)]
    (zmq/bind frontend "ipc://frontend.ipc")
    (zmq/bind backend "ipc://backend.ipc")
    (dotimes [i client-nbr]
      (doto (Thread. (client i))
        (.setDaemon true)
        .start))
    (dotimes [i worker-nbr]
      (doto (Thread. (worker i))
        (.setDaemon true)
        .start))
    ;; Logic of LRU loop
    ;; - Poll backend always, frontend only if 1+ worker ready
    ;; - If worker replies, queue worker as ready and forward reply
    ;; to client if necessary
    ;; - If client requests, pop next worker and send request to it
    (let [clients-left (atom client-nbr)
          worker-queue (atom clojure.lang.PersistentQueue/EMPTY)]
      (while (and (not (.isInterrupted (Thread/currentThread)))
                  (> @clients-left 0))
        (let [items (zmq/poller ctx)
              backend-index (zmq/register items backend :pollin)
              frontend-index (if-not (empty? @worker-queue)
                               (zmq/register items frontend :pollin))]
          (zmq/poll items -1)
          ;; Handle worker activity on background
          (if (zmq/check-poller items backend-index :pollin)
            ;; Queue worker address for LRU routing
            (let [worker-id (zmq/receive-str backend)
                  _ (zmq/receive-str backend)
                  client-id (zmq/receive-str backend)]
              (swap! worker-queue conj worker-id)
              (if (not= "READY" client-id)
                (let [_ (zmq/receive backend)
                      reply (zmq/receive-str backend)]
                  (zmq/send-str frontend client-id zmq/send-more)
                  (zmq/send-str frontend "" zmq/send-more)
                  (zmq/send-str frontend reply)
                  (swap! clients-left dec)))))
          (if (and frontend-index (zmq/check-poller items frontend-index :pollin))
            (let [client-id (zmq/receive-str frontend)
                  _ (zmq/receive frontend)
                  request (zmq/receive-str frontend)
                  worker-id (peek @worker-queue)]
              (swap! worker-queue pop)
              (zmq/send-str backend worker-id zmq/send-more)
              (zmq/send-str backend "" zmq/send-more)
              (zmq/send-str backend client-id zmq/send-more)
              (zmq/send-str backend "" zmq/send-more)
              (zmq/send-str backend request))))))
    (.close frontend)
    (.close backend)
    (.term ctx)))
