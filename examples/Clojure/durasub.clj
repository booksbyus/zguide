(ns durasub
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;
; Durable subscriber
; Isaiah Peng <issaria@gmail.com>
;

(defn -main []
  (let [ctx (mq/context 1)
        subscriber (mq/socket ctx mq/sub)
        sync (mq/socket ctx mq/push)]
    (mq/identify subscriber "Hello")
    (mq/connect subscriber "tcp://localhost:5565")
    (mq/connect sync "tcp://localhost:5564")
    (mq/subscribe subscriber "")
    (mq/send sync "\u0000")
    (loop [msg (mq/recv-str subscriber)]
      (println msg)
      (if (not= "END" msg) 
        (recur (mq/recv-str subscriber))))
    (.close subscriber)
    (.close sync)
    (.term ctx)))
