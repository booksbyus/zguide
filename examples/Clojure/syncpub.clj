(ns storm_demo.syncpub
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq]))

;
; Synchronized publisher
;

(defn main []
  (let [ctx (mq/context 1)
        publisher (mq/socket ctx mq/pub)
        syncservice (mq/socket ctx mq/rep)]
    (mq/bind publisher "tcp://*:5561")
    (mq/bind syncservice "tcp://*:5562")
    (println "Waiting for subscribers")
    (doseq [i (range 10)]
      (mq/recv syncservice)
      (mq/send syncservice (.getBytes "\u0000")))
    (println "Broadicasting messages")
    (doseq [i (range 1000000)]
      (mq/send publisher (.getBytes "Rhubarb\u0000")))
    (mq/send publisher (.getBytes "END\u0000"))
    (.close publisher)
    (.close syncservice)
    (.term ctx)))
