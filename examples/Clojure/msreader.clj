(ns storm_demo.msreader
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq]))

;
; Reading from multiple sockets in Clojure
; Isaiah Peng <issaria@gmail.com>
;

(defn main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        subscriber (mq/socket ctx mq/sub)]
    (mq/connect receiver "tcp://localhost:5557")
    (mq/connect subscriber "tcp://localhost:5556")
    (mq/subscribe subscriber (.getBytes "10001 "))
    (while true
      (while (not (nil? (mq/recv receiver 0))))
      (while (not (nil? (mq/recv subscriber 0))))
      (Thread/sleep 1000000))))
