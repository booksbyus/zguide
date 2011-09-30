(ns storm_demo.rrserver
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]]))

;                                                 
; Hello World server                               
; Connects REP socket to tcp://*:5560              
; Expects "Hello" from client, replies with "World"
; Isaiah Peng <issaria@gmail.com>
;

(defn main []
  (let [ctx (mq/context 1)
        responder (mq/socket ctx mq/rep)]
    (mq/connect responder "tcp://localhost:5560")
    (while true
      (let [string (-> responder mq/recv String. trim)]
        (println (format "Received request: [%s]." string))
        (Thread/sleep 1)
        (mq/send responder (.getBytes "World\u0000"))))
    ; We never get here but clean anyhow
    (.close responder)
    (.term ctx)))
