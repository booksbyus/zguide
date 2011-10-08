(ns examples.syncsub
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]]))

(defn -main [& args]
  (let [ctx (mq/context 1)
        subscriber (mq/socket ctx mq/sub)
        syncclient (mq/socket ctx mq/req)
        update-nbr (atom 0)]
    (mq/connect subscriber "tcp://localhost:5561")
    (mq/connect syncclient "tcp://*:5562")
    (mq/subscribe subscriber (.getBytes ""))
    (mq/send syncclient (.getBytes ""))
    (mq/recv subscriber)
    (loop [string (-> subscriber mq/recv String. trim)]
      (if (not= "END" string)
        (swap! update-nbr #(+ % 1))
        (recur (-> subscriber mq/recv String. trim))))
    (println (format "Received %d updates." @update-nbr))
    (.close subscriber)
    (.close syncclient)
    (.term ctx)))
