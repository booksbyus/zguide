(ns spqueue
  (:require [zeromq.zmq :as zmq]
            [zmsg :refer :all]))

;;
;;  Simple Pirate broker
;;  This is identical to load-balancing pattern, with no reliability
;;  mechanisms. It depends on the client for recovery. Runs forever.
;;

(def worker-ready "\001")

(defn -main []
  (let [ctx (zmq/context 1)
        frontend (doto (zmq/socket ctx :router)
                   (zmq/bind "tcp://*:5555"))
        backend (doto (zmq/socket ctx :router)
                  (zmq/bind "tcp://*:5556"))]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (.close frontend)
                                 (.close backend)
                                 (.term ctx))))
    (let [worker-queue (atom clojure.lang.PersistentQueue/EMPTY)]
      (while true
        (let [items (zmq/poller ctx)
              backend-index (zmq/register items backend :pollin)
              frontend-index (if-not (empty? @worker-queue)
                               (zmq/register items frontend :pollin))]
          (zmq/poll items -1)
          ;; Handle worker activity on background
          (if (zmq/check-poller items backend-index :pollin)
            ;; Queue worker address for LRU routing
            (let [msg (receive-msg backend)
                  worker-id (msg-id msg)
                  msg (msg-unwrap msg)]
              (swap! worker-queue conj worker-id)
              (if (not= worker-ready (msg-data msg))
                (send-msg frontend msg))))
          (if (and frontend-index (zmq/check-poller items frontend-index :pollin))
            (let [ msg (receive-msg frontend)
                  worker-id (peek @worker-queue)]
              (swap! worker-queue pop)
              (send-msg backend (msg-wrap worker-id msg)))))))))
