(ns rrbroker
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [org.zeromq ZMQ$Poller]))

;;
;; Simple request-reply broker
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        frontend (mq/socket ctx mq/router)
        backend (mq/socket ctx mq/dealer)
        items (.poller ctx 2)]
    (mq/bind frontend "tcp://*:5559")
    (mq/bind backend "tcp://*:5560")
    (.register items frontend ZMQ$Poller/POLLIN)
    (.register items backend ZMQ$Poller/POLLIN)
    (while (not (.isInterrupted (Thread/currentThread)))
      (.poll items 250000)
      (if (.pollin items 0)
        (loop [#^String message (mq/recv-str frontend)]
          (let [more (.hasReceiveMore frontend)]
            (if more
              (do
                (mq/send-more backend message)
                (recur (mq/recv-str frontend)))
              (mq/send backend message)))))
      (if (.pollin items 1)
        (loop [#^String message (mq/recv-str backend)]
          (let [more (.hasReceiveMore backend)]
            (if more
              (do
                (mq/send-more frontend message)
                (recur (mq/recv-str backend)))
              (mq/send frontend message))))))
    (.close frontend)
    (.close backend)
    (.term ctx)))
