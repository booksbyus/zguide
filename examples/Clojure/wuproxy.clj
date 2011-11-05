(ns wuproxy
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Weather proxy device
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn main []
  (let [ctx (mq/context 1)
        frontend (mq/socket ctx mq/sub)
        backend (mq/socket ctx mq/pub)]
    (mq/connect frontend "tcp://192.168.55.210:5556")
    (mq/bind backend "tcp://10.1.1.0:8100")
    ;; Subscribe on everything
    (mq/subscribe frontend "")
    (while (not (.isInterrupted (Thread/currentThread)))
      (while true
        (let [message (mq/recv frontend)
              more (.hasReceiveMore frontend)]
          (if more
            (mq/send backend message mq/sndmore)
            (mq/send backend message 0)))))))
