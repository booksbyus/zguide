;; Task ventilator
;; Binds PUSH socket to tcp://localhost:5557
;; Sends batch of tasks to workers via that socket

(ns taskvent
  (:require [zeromq.zmq :as zmq]))

(defn work [socket]
  ;; Random workload from 1 to 100 ms
  (let [workload (inc (rand-int 100))]
    (zmq/send-str socket (str workload))
    workload))

(defn -main []
  (let [context (zmq/zcontext 1)]
    (with-open [sender (doto (zmq/socket context :push)
                         (zmq/bind "tcp://*:5557"))
                sink (doto (zmq/socket context :push)
                       (zmq/connect "tcp://127.0.0.1:5558"))]
    (println "Press Enter when the workers are ready: ")
    (read-line)
    (println "Sending tasks to workers…")
      (zmq/send-str sink "0")
      (let [times (repeatedly 100 (partial work sender))]
        (printf "Total expected cost: %d msec\n" (apply + times))))))
