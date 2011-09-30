(ns storm_demo.taskvent
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq])
  (:import [java.util Random]))
;
; Task ventilator in Clojure
; Isaiah Peng <issaria@gmail.com>
;

(defn main []
  (let
      ; Socket to send messages on
      [cxt (mq/context 1)
       sender (mq/socket cxt mq/push)
       ; Socket to send messages on
       sink (mq/socket cxt mq/push)
       srandom (Random. (System/currentTimeMillis))
       ; Total expected cost in msecs
       total-msec (atom 0)]
    (mq/bind sender "tcp://*:5557")
    (mq/connect sink "tcp://localhost:5558")
    (println "Press Enter when the workers are ready: ")
    (.read System/in)
    (println "Sending tasks to workers...\n")
    ; The first message is "0" and signals start of batch
    (mq/send sink (.getBytes "0\u0000" 0))
    (doseq [i (range 100)]
      (let [workload (-> srandom (.nextInt 100) (+ 1))
            string (format "%d\u0000" workload)]
        (swap! total-msec #(+ % workload))
        (print (str workload "."))
        (mq/send sender (.getBytes string) 0)))
    (println (str "Total expected cost: " @total-msec " msec"))
    (.close sink)
    (.close sender)
    (.term cxt)
    ; Give 0MQ time to deliver
    (Thread/sleep 1000)))
