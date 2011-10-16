(ns peering1
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [java.util Random]))

;;
;; Broker peering simulation (part 1)
;; Prototypes the state flow
;; Isaiah Peng <issaria@gmail.com>

(defn -main
  "First argument is this broker's name, other arguments are our peer's name."
  [& args]
  (if (< (count args) 2)
    (do
      (println "syntax: peering me {you}...")
      (System/exit)))
  (let [srandom (Random. (System/currentTimeMillis))
        self (nth args 0)
        ctx (mq/contenxt 1)
        statebe (mq/socket ctx mq/pub)
        statefe (mq/socket ctx mq/sub)]
    (println (format "I: preparing broker at %s..." self))
    (mq/bind statebe (format "ipc://%s-state.ipc" self))
    ;; Connect statefe to all peers
    (doseq [peer (rest args)]
      (println (format "I: connecting to state backend at '%s'" peer))
      (mq/connect statefe (format "ipc://%s-state.ipc", peer)))
    ;; Send out status messages to peers, and collect from peers, the zmq_poll timeout define our own heatbeating
    (while true
      (let [poller (.poller ctx 1)]
        (.register poller statefe (ZMQ$Poller/POLLIN))
        (.poll poller 1000000)
        (if (.pollin poller 0)
          (let [addr body (mq/recv-all statefe)]
            (println (format "%s - %s workers free" addr body)))
          (do
            (mq/sndmore statefe self)
            (mq/send statefe (.nextInt srandom 10))))))))
