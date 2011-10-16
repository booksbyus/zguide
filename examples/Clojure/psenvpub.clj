(ns psenvpub
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Pubsub envelope publisher
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        publisher (mq/socket ctx mq/pub)]
    (mq/bind publisher "tcp://*:5563")
    (while true
      ;; Write two messages, each with an envelope and content
      (mq/send publisher "A\u0000" mq/sndmore)
      (mq/send publisher "We don't want to see this.\u0000")
      (mq/send publisher "B\u0000" mq/sndmore)
      (mq/send publisher "We would like to see this.\u0000")
      (Thread/sleep 1000))
    (.close publisher)
    (.term ctx)))
