(ns examples.psenvsub
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]]))

;
; Pubsub envelope subscriber
; Isaiah Peng <issaria@gmail.com>
;

(defn -main []
  (let [ctx (mq/context 1)
        subscriber (mq/socket ctx mq/sub)]
    (mq/connect subscriber "tcp://localhost:5563")
    (mq/subscribe subscriber (.getBytes "B"))
    (while true
      (let [; Read envelope with address
            address (-> subscriber mq/recv String. trim)
            ; Read message contents
            contents (-> subscriber mq/recv String. trim)]
       (println (format "%s : %s" address contents))))))
