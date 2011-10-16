(ns msreader
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Reading from multiple sockets
;; This version uses a simple recv loop
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        subscriber (mq/socket ctx mq/sub)]
    (mq/connect receiver "tcp://localhost:5557")
    (mq/connect subscriber "tcp://localhost:5556")
    (mq/subscribe subscriber "10001 ")
    ;; Process messages from both sockets
    ;; We prioritize traffic from the task ventilator
    (while true
      ;; Process any waiting tasks
      (while (not (nil? (mq/recv-str receiver))))
      ;; Process any waiting weather updates
      (while (not (nil? (mq/recv-str subscriber))))
      ;; No actitivity, so sleep for a msec
      (Thread/sleep 1))))
