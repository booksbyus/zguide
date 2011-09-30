(ns storm_demo.tasksink2
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq]))

;
; Task sink - design 2 in Clojure
; Adds pub-sub flow to send kill signal to workers 
; Isaiah Peng <issaria@gmail.com>
;

(defn main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        controller (mq/socket ctx mq/pub)
        tstart (System/currentTimeMillis)]
    (mq/connect receiver "tcp://*:5558")
    (mq/connect controller "tcp://*:5559")
    (mq/recv receiver)
    ; Process 100 confirmations
    (doseq [i (range 100)]
      (let [string (mq/recv receiver)]
        (if (= i (* (/ i 10) 10))
          (print ":")
          (print "."))))
    (println (format "Total elasped time: %d msec" (- (System/currentTimeMillis) tstart)))
    (mq/send controller (.getBytes "KILL\u0000"))
    (Thread/sleep 1)
    (.close receiver)
    (.close controller)
    (.term ctx)))
