;; Binds PULL socket to tcp://localhost:5558
;; Collects results from workers via that socket

(ns tasksink
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [context (zmq/zcontext 1)]
    (with-open [receiver (doto (zmq/socket context :pull)
                           (zmq/bind "tcp://*:5558"))]
      ;; Wait for start of batch
      (zmq/receive receiver)
      (let [tstart (System/currentTimeMillis)]
        (dotimes [i 100]
          (zmq/receive receiver)
          (if (zero? (mod i 10))
            (print ":")
            (print ".")))
        ;; Calculate and report duration of batch
        (println "Total elapsed time:" (- (System/currentTimeMillis) tstart) " msec")))))
