(ns storm_demo.rrbroker
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:import [org.zeromq ZMQ$Poller]))

;
; Simple request-reply broker
;

(defn main []
  (let [ctx (mq/context 1)
        frontend (mq/socket ctx mq/xrep)
        backend (mq/socket ctx mq/xreq)
        items (.poller ctx 2)]
    (mq/connect frontend "tcp://localhost:5559")
    (mq/connect backend "tcp://localhost:5560")
    (.register frontend ZMQ$Poller/POLLIN)
    (.register backend ZMQ$Poller/POLLIN)
    (while (not (.isInterrupted (Thread/currentThread)))
      (.poll items)
      (if (.pollin items 0)
        (loop [message (mq/recv frontend)
               more (.hasReceiveMore frontend)]
          (if more
            (do
              (mq/send backend message mq/sndmore)
              (recur (mq/recv frontend) (.hasReceiveMore frontend)))
            (mq/send backend message 0))))
      (if (.pollin items 1)
        (loop [message (mq/recv backend)
               more (.hasReceiveMore backend)]
          (if more
            (do
              (mq/send frontend message mq/sndmore)
              (recur (mq/recv backend) (.hasReceiveMore backend)))
            (mq/send frontend message 0)))))
    (.close frontend)
    (.close backend)
    (.term ctx)))
