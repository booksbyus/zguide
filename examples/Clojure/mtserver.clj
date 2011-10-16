(ns mtserver
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [org.zeromq ZMQ$Context ZMQQueue]))

;;
;; Multithreaded Hello World server
;; Isaiah Peng <issaria@gmail.com>
;;

(defrecord Worker [^ZMQ$Context ctx]
  Runnable
  (run [this]
    (let [receiver (mq/socket ctx mq/rep)]
      (mq/connect receiver "inproc://workers")
      (while true
        (let [string (mq/recv-str receiver)]
          (println (format "Received request: [%s]." string))
          (Thread/sleep 1)
          (mq/send receiver "World\u0000")))
      (.close receiver))))

(defn -main []
  (let [ctx (mq/context 1)
        clients (mq/socket ctx mq/router)
        workers (mq/socket ctx mq/dealer)]
    (mq/bind clients "tcp://*:5555")
    (mq/bind workers "inproc://workers")
    (dotimes [i 5]
      (-> ctx Worker. Thread. .start))
    (let [queue (ZMQQueue. ctx clients workers)]
      (.run queue))
    ;; We never get here..
    (.close clients)
    (.close workers)
    (.term ctx)))
