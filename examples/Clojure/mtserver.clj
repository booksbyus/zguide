(ns storm_demo.mtserver
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]])
  (:import [org.zeromq ZMQ$Context ZMQQueue]))

;
; Multithreaded Hello World server
; Isaiah Peng <issaria@gmail.com>
;

(defrecord Worker [^ZMQ$Context ctx]
  Runnable
  (run [this]
    (let [receiver (mq/socket ctx mq/rep)]
      (mq/connect receiver "inproc://workers")
      (while true
        (let [string (-> receiver mq/recv String. trim)]
          (println (format "Received request: [%s]." string))
          (Thread/sleep 1)
          (mq/send receiver (.getBytes "World\u0000"))))
      (.close receiver))))

(defn main []
  (let [ctx (mq/context 1)
        clients (mq/socket ctx mq/xrep)
        workers (mq/socket ctx mq/xreq)]
    (mq/bind clients "tcp://*:5555")
    (mq/bind workers "inproc://workers")
    (doseq [i (range 5)]
      (-> ctx Worker. Thread. .start))
    (ZMQQueue. ctx clients workers)
    ; We never get here..
    (.close clients)
    (.close workers)
    (.term ctx)))
