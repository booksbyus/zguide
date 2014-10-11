;; Reading from multiple sockets
;; This version uses zmq_poll()

(ns mspoller
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [context (zmq/zcontext)
        poller (zmq/poller context 2)]
    (with-open [receiver (doto (zmq/socket context :pull)
                           (zmq/connect "tcp://127.0.0.1:5557"))
                subscriber (doto (zmq/socket context :sub)
                             (zmq/connect "tcp://127.0.0.1:5556")
                             (zmq/subscribe "10001"))]
      (zmq/register poller receiver :pollin)
      (zmq/register poller subscriber :pollin)
      (while (not (.. Thread currentThread isInterrupted))
        (zmq/poll poller)
        (when (zmq/check-poller poller 0 :pollin)
          (let [msg (zmq/receive receiver)]
            ;; Process task
            ))
        (when (zmq/check-poller poller 1 :pollin)
          (let [msg (zmq/receive subscriber)]
            ;; Process weather update
            ))))))
