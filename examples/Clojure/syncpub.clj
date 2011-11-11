(ns syncpub
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Synchronized publisher
;;
;; Isaiah Peng <issaria@gmail.com>

(defn -main []
  (let [ctx (mq/context 1)
        publisher (mq/socket ctx mq/pub)
        syncservice (mq/socket ctx mq/rep)
        subscribers-expected 10]
    (mq/bind publisher "tcp://*:5561")
    (mq/bind syncservice "tcp://*:5562")
    ;; Get synchronization from subscribers
    (println "Waiting for subscribers")
    (dotimes [i subscribers-expected]
      ;; - wait for synchronization request
      (mq/recv syncservice)
      ;; - send synchronization reply
      (mq/send syncservice "\u0000"))
    ;; Now broadcast exactly 1M updates followed by END
    (println "Broadicasting messages")
    (dotimes [i 1000000]
      (mq/send publisher "Rhubarb\u0000"))
    (mq/send publisher "END\u0000")
    (.close publisher)
    (.close syncservice)
    (.term ctx)))
