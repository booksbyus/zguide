(ns storm_demo.tasksink
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]]))
;
; Task sink in Clojure
; Isaiah Peng <issaria@gmail.com>
;

(defn main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)]
    (mq/bind receiver "tcp://*:5558")
    ; Wait for start of batch
    (mq/recv receiver 0)
    (let [tstart (System/currentTimeMillis)
          total-msec (atom 0)]
      (doseq [i (range 100)]
        (let [string (-> receiver (mq/recv 0) String. trim)]
          (if (= i (int (-> i (/ 10) (* 10 ))))
            (print ":")
            (print "."))))
      ; Calculate and report duration of batch
      (println (str "Total elapsed time:" (- (System/currentTimeMillis) tstart) " msec"))
      (.close receiver)
      (.term ctx))))
