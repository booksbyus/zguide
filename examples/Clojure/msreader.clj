;; Reading from multiple sockets
;; This version uses a simple recv loop

(ns msreader
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [context (zmq/zcontext)]
    (with-open [receiver (doto (zmq/socket context :pull)
                           (zmq/connect "tcp://127.0.0.1:5557"))
                subscriber (doto (zmq/socket context :sub)
                             (zmq/connect "tcp://127.0.0.1:5556")
                             (zmq/subscribe "10001"))]
      (while (not (.. Thread currentThread isInterrupted))
        (while (not (zmq/receive receiver zmq/dont-wait)))
        (while (not (zmq/receive subscriber zmq/dont-wait)))
        (Thread/sleep 1)))))
