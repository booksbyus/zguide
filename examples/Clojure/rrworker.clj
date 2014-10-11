;; Hello World worker
;; Connects REP socket to tcp://*:5560
;; Expects "Hello" from client, replies with "World"

(ns rrworker
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [context (zmq/zcontext)]
    (with-open [responder (doto (zmq/socket context :rep)
                            (zmq/connect "tcp://127.0.0.1:5560"))]
      (while (not (.. Thread currentThread isInterrupted))
        (let [string (zmq/receive-str responder)]
          (printf "Received request: [%s]\n", string)
          ;; Do some work
          (Thread/sleep 1000)
          (zmq/send-str responder "World"))))))
