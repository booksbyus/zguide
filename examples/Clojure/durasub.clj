(ns examples.durasub
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]]))

;
; Durable subscriber
; Isaiah Peng <issaria@gmail.com>
;

(defn -main []
  (let [ctx (mq/context 1)
        subscriber (mq/socket ctx mq/sub)
        sync (mq/socket ctx mq/push)]
    (mq/connect subscriber "tcp://localhost:5565")
    (mq/connect sync "tcp://localhost:5564")
    (.setIdentity subscriber (.getBytes "hello"))
    (mq/subscribe subscriber (.getBytes ""))
    (mq/send sync (.getBytes "\u0000"))
    (loop [msg (-> subscriber mq/recv String. trim)]
      (println msg)
      (if (not= "END" msg) 
        (recur (-> subscriber mq/recv String. trim))))))
