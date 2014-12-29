(ns spworker
  (:require [zeromq.zmq :as zmq]
            [zmsg :refer :all]))

(def worker-ready "\001")

(defn -main []
  (let [ctx (zmq/zcontext)
        identity (format "%04X-%04X" (rand-int 0x10000) (rand-int 0x10000))
        worker (doto (zmq/socket ctx :req)
                 (zmq/set-identity (.getBytes identity))
                 (zmq/connect "tcp://localhost:5556"))]
    (println (format "I: (%s) worker ready" identity))
    (zmq/send-str worker worker-ready)
    (try
      (doseq [cycles (iterate inc 1)]
        (let [msg (receive-msg worker)]
          (cond
           (and (> cycles 3) (zero? (rand-int 5)))
           (do (println "I: (%s) simulating a crash" identity)
               (throw (Exception. "crashing")))
           (and (> cycles 3) (zero? (rand-int 5)))
           (do (println (format "I: (%s) simulating CPU overload" identity))
               (Thread/sleep 3000))
           :else nil)
          (println (format "I: (%s) normal reply" identity))
          (Thread/sleep 1000)
          (send-msg worker msg)))
      (catch Exception _ nil))
    (zmq/destroy ctx)))
