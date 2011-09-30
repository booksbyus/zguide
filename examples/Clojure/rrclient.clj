(ns storm_demo.rrclient
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq]))

;                                             
; Hello World client                           
; Connects REQ socket to tcp://localhost:5559  
; Sends "Hello" to server, expects "World" back
;

(defn main []
  (let [ctx (mq/context 1)
        requester (mq/socket ctx mq/req)]
    (mq/connect requester "tcp://localhost:5559")
    (doseq [i (range 10)]
      (mq/send requester (.getBytes "Hello\u0000"))
      (let [string (mq/recv requester)]
        (println (format "Received reply %d %s" i string))))
    (.close requester)
    (.term ctx)))

