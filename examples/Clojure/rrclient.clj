(ns rrclient
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [context (zmq/zcontext)]
    (with-open [requester (doto (zmq/socket context :req)
                            (zmq/connect "tcp://127.0.0.1:5559"))]
      (dotimes [i 10]
        (zmq/send-str requester "Hello")
        (let [string (zmq/receive-str requester)]
          (printf "Received reply %d [%s]\n" i string))))))
