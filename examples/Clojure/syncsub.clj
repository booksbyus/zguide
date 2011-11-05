(ns syncsub
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Synchronized subscriber
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        subscriber (mq/socket ctx mq/sub)
        syncclient (mq/socket ctx mq/req)
        update-nbr (atom 1)]
    (mq/connect subscriber "tcp://localhost:5561")
    (mq/connect syncclient "tcp://*:5562")
    (mq/subscribe subscriber "")
    (mq/send syncclient "")
    (mq/recv subscriber)
    (loop [string (mq/recv-str subscriber)]
      (if (not= "END" string)
        (do
          (swap! update-nbr inc)
          (recur (mq/recv-str subscriber)))))
    (println (format "Received %d updates." @update-nbr))
    (.close subscriber)
    (.close syncclient)
    (.term ctx)))
