(ns examples.psenvpub
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq]))

;
; Pubsub envelope publisher
; Isaiah Peng <issaria@gmail.com>
;

(defn -main []
  (let [ctx (mq/context 1)
        publisher (mq/socket ctx mq/pub)]
    (mq/bind publisher "tcp://*:5563")
    (while true
      ; Write two messages, each with an envelope and content
      (mq/send publisher (.getBytes "A\u0000") mq/sndmore)
      (mq/send publisher (.getBytes "We don't want to see this.\u0000") 0)
      (mq/send publisher (.getBytes "B\u0000") mq/sndmore)
      (mq/send publisher (.getBytes "We would like to see this.\u0000") 0)
      (Thread/sleep 1000))
    (.close publisher)
    (.term ctx)))
