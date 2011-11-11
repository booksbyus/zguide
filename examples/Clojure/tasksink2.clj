(ns tasksink2
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Task sink - design 2 in Clojure
;; Adds pub-sub flow to send kill signal to workers 
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        controller (mq/socket ctx mq/pub)
        tstart (System/currentTimeMillis)]
    (mq/bind receiver "tcp://*:5558")
    (mq/bind controller "tcp://*:5559")
    ;; Wait for start of batch
    (mq/recv receiver)
    ;; Process 100 confirmations
    (dotimes [i 100]
      (let [string (mq/recv receiver)]
        (if (= i (* (int (/ i 10)) 10))
          (print ":")
          (print "."))))
    (println (format "Total elasped time: %d msec" (- (System/currentTimeMillis) tstart)))
    (mq/send controller "KILL\u0000")
    ;; Finished
    (Thread/sleep 1) ; Give 0MQ time to deliver
    (.close receiver)
    (.close controller)
    (.term ctx)))
