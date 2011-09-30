(ns storm_demo.msqueue
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:import [org.zeromq ZMQQueue]))

;
; Simple message queuing broker 
; Same as request-reply broker but using QUEUE device
; Isaiah Peng <issaria@gmail.com>
;

(defn main []
  (let [ctx (mq/context 1)
        ; Socket facing clients
        frontend (mq/socket ctx mq/xrep)
        ; Socket facing services
        backend (mq/socket ctx mq/xreq)]
    (mq/connect frontend "tcp://*:5559")
    (mq/connect backend "tcp://*:5560")
    (ZMQQueue. ctx frontend backend)
    ; We never get there...
    (.close frontend)
    (.close backend)
    (.term ctx)))
