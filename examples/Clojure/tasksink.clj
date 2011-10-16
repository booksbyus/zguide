(ns tasksink
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))
 
;;
;; Task sink
;; Binds PULL socket to tcp://localhost:5558
;; Collects results from workers via that socket
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)]
    (mq/bind receiver "tcp://*:5558")
    ; Wait for start of batch
    (mq/recv receiver 0)
    (let [tstart (System/currentTimeMillis)]
      (dotimes [i 100]
        (mq/recv receiver)
        (if (= i (-> (int (/ i 10)) (* 10 )))
          (print ":")
          (print ".")))
      ;; Calculate and report duration of batch
      (println (str "Total elapsed time:" (- (System/currentTimeMillis) tstart) " msec"))
      (.close receiver)
      (.term ctx))))
