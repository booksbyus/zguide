(ns durapub2
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))
;
; Publisher for durable subscriber
; Isaiah Peng <issaria@gmail.com>
;

(defn -main []
  (let [ctx (mq/context 1)
        publisher (mq/socket ctx mq/pub)
        sync (mq/socket ctx mq/pull)]
    (mq/bind publisher "tcp://*:5565")
    (mq/bind sync "tcp://*:5564")
    ; Prevent publisher overflow from slow subscribers
    (.setHWM publisher 1)
    ; Specify swap space in bytes, this covers all subscribers
    (.setSwap publisher 25000000)
                                        ; Wait for synchronization request
    (mq/recv sync)
    ; Now broadcast exactly 10 updates with pause
    (dotimes [i 10]
      (mq/send publisher (format "Update %d" i))
      (Thread/sleep 1000))
    (mq/send publisher "END")
    ; Give 0MQ to flush out
    (Thread/sleep 10000)
    (.close publisher)
    (.close sync)
    (.term ctx)))
