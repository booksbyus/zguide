(ns examples.durapub
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq]))
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
    (mq/recv sync)
    (doseq [i (range 10)]
      (mq/send publisher (-> "Update %d\u0000" (format i) .getBytes) 0)
      (Thread/sleep 1000))
    (mq/send publisher (.getBytes "END\u0000"))
    (.close publisher)
    (.close sync)
    (.term ctx)))
