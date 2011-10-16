(ns rrserver
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;                                                 
;; Hello World server                               
;; Connects REP socket to tcp://*:5560              
;; Expects "Hello" from client, replies with "World"
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        responder (mq/socket ctx mq/rep)]
    (mq/connect responder "tcp://localhost:5560")
    (while true
      (let [string (mq/recv-str responder)]
        (println (format "Received request: [%s]." string))
        ;; Do some work
        (Thread/sleep 1000)
        (mq/send responder "World\u0000")))
    ;; We never get here but clean anyhow
    (.close responder)
    (.term ctx)))
