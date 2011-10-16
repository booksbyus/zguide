(ns psenvsub
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Pubsub envelope subscriber
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        subscriber (mq/socket ctx mq/sub)]
    (mq/connect subscriber "tcp://localhost:5563")
    (mq/subscribe subscriber "B")
    (while true
      (let [;; Read envelope with address
            address (mq/recv-str subscriber)
            ;; Read message contents
            contents (mq/recv-str subscriber)]
       (println (format "%s : %s" address contents))))))
