(ns msgqueue
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Simple message queuing broker
;; Same as request-reply broker but using QUEUE device
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        frontend (mq/socket ctx mq/router)
        backend (mq/socket ctx mq/dealer)]
    (mq/bind frontend "tcp://*:5559")
    (mq/bind backend "tcp://*:5560")
    ;; Start built-in device
    (let [queue (mq/queue ctx frontend backend)]
      (.run queue))
    ;; We never get here
    (.close frontend)
    (.close backend)
    (.term ctx)))
