(ns storm_demo.mspoller
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:import [org.zeromq ZMQ$Poller]))

(defn main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        subscriber (mq/socket ctx mq/sub)
        items (.poller ctx 2)]
    (mq/connect receiver "tcp://localhost:5557")
    (mq/connect subscriber "tcp://localhost:5556")
    (mq/subscribe subscriber (.getBytes "10001 "))
    (.register receiver ZMQ$Poller/POLLIN)
    (.register subscriber ZMQ$Poller/POLLIN)
    (while true
      (.poll items)
      (if (.pollin items 0)
        (mq/recv receiver))
      (if (.pollin items 1)
        (mq/recv subscriber)))))
