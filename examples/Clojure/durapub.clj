(ns durapub
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Publisher for durable subscriber
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        publisher (mq/socket ctx mq/pub)
        sync (mq/socket ctx mq/pull)]
    (mq/bind publisher "tcp://*:5565")
    (mq/bind sync "tcp://*:5564")
    (mq/recv sync)
    (dotimes [i 10]
      (mq/send publisher (format "Update %d\u0000" i))
      (Thread/sleep 1000))
    (mq/send publisher "END\u0000")
    ; Give 0MQ to flush out
    (Thread/sleep 10000)
    (.close publisher)
    (.close sync)
    (.term ctx)))
