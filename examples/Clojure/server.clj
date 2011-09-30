(ns storm_demo.server
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq]))

(defn run []
  (let [sock (-> 1 mq/context (mq/socket mq/rep))]
    (mq/bind sock "tcp://*:5555")
    (loop []
      (let [req (recv sock)]
        (println (str "Received request: " (String. req)))
        (Thread/sleep 1)
        (mq/send sock (.getBytes "World")))
      (recur ))))
