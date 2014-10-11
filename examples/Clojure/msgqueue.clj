;; Simple message queuing broker
;; Same as request-reply broker but using QUEUE device

(ns msgqueue
  (:require [zeromq
             [device :as device]
             [zmq :as zmq]]))

(defn -main []
  (let [context (zmq/zcontext)
        poller (zmq/poller context 2)]
    (with-open [frontend (doto (zmq/socket context :router)
                           (zmq/bind "tcp://*:5559"))
                backend (doto (zmq/socket context :dealer)
                          (zmq/bind "tcp://*:5560"))]
      (device/proxy context frontend backend))))
