(ns lpserver
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [ctx (zmq/zcontext)
        server (doto (zmq/socket ctx :rep)
                 (zmq/bind "tcp://*:5555"))]
    (try
      (doseq [cycles (iterate inc 1)]
        (let [req (zmq/receive-str server)]
          (cond
           (and (> cycles 3) (zero? (rand-int 3)))
           (do (println "I: simulating a crash")
               (throw (Exception. "crashing")))
           (and (> cycles 3) (zero? (rand-int 3)))
           (do (println "I: simulating CPU overload")
               (Thread/sleep 2000))
           :else nil)
          (println (format "I: normal request (%s)" req))
          (Thread/sleep 1000)
          (zmq/send-str server req)))
      (catch Exception _ nil))
    (zmq/destroy ctx)))
