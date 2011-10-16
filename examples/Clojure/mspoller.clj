(ns mspoller
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [org.zeromq ZMQ$Poller]))

;;
;; Reading from multiple sockets
;; This version uses zmq_poll()
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        subscriber (mq/socket ctx mq/sub)
        items (.poller ctx 2)]
    ;; Connect to task ventilator
    (mq/connect receiver "tcp://localhost:5557")
    ;; Connect to weather server
    (mq/connect subscriber "tcp://localhost:5556")
    (mq/subscribe subscriber "10001 ")
    ;; Initialize poll set
    (.register items receiver ZMQ$Poller/POLLIN)
    (.register items subscriber ZMQ$Poller/POLLIN)
    ;; Process messages from both sockets
    (while true
      (.poll items)
      (if (.pollin items 0)
        ;; Process task
        (let [task (mq/recv-str receiver)]
          (println (format "task: %s" task))))
      (if (.pollin items 1)
        ;; Process weather update
        (let [weather (mq/recv-str subscriber)]
          (println (format "weather: %s" weather)))))))
