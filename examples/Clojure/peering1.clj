(ns peering1
  (:require [zeromq.zmq :as zmq])
  (:import [java.util Random]))

;; Broker peering simulation (part 1)
;; Prototypes the state flow

(defn -main
  "First argument is this broker's name, other arguments are our peer's name."
  [& [self & peers :as args]]
  (when (empty? peers)
    (println "syntax: peering me {you}...")
    (System/exit 0))
  (println (format "I: preparing broker at %s..." self))
  (let [srandom (Random. (System/currentTimeMillis))
        ctx (zmq/context 1)
        ;; Bind state backend to endpoint
        statebe (doto (zmq/socket ctx :pub)
                  (zmq/bind (format "ipc://%s-state.ipc" self)))
        statefe (doto (zmq/socket ctx :sub)
                  (zmq/subscribe ""))]



    ;; Connect statefe to all peers
    (doseq [peer peers]
      (println (format "I: connecting to state backend at '%s'" peer))
      (zmq/connect statefe (format "ipc://%s-state.ipc", peer)))

    ;; The main loop sends out status messages to peers, and collects
    ;; status messages back from peers. The zmq_poll timeout defines
    ;; our own heartbeat:

    (let [poller (doto (zmq/poller ctx 1)
                   (zmq/register statefe :pollin))]
      (while (not= -1 (zmq/poll poller 1000))
        (if (zmq/check-poller poller 0 :pollin)
          (let [[addr body] (mapv #(String. %)
                                  (zmq/receive-all statefe))]
            (println (format "%s - %s workers free" addr body)))
          (do
            (zmq/send-str statebe self zmq/send-more)
            (zmq/send-str statebe (format "%d" (.nextInt srandom 10)))))))))
