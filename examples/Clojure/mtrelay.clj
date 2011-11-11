(ns mtrelay
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [org.zeromq ZMQ$Context]))

;;
;; Multithreaded relay in Clojure
;; Isaiah Peng <issaria@gmail.com>
;;

(defrecord Step1 [^ZMQ$Context ctx]
  Runnable
  (run [this]
    (let [xmitter (mq/socket ctx mq/pair)]
      (mq/connect xmitter "inproc://step2")
      (println "Step 1 is ready, signaling step 2")
      (mq/send xmitter "READY\u0000")
      (.close xmitter))))

(defrecord Step2 [^ZMQ$Context ctx]
  Runnable
  (run [this]
    (let [receiver (mq/socket ctx mq/pair)
          xmitter (mq/socket ctx mq/pair)]
      (mq/bind receiver "inproc://step2")
      (-> ctx Step1. Thread. .start)
      ;; Wait for signal and pass it on
      (mq/recv-str receiver)
      (mq/connect xmitter "inproc://step3")
      (println "Step 2 is ready, signaling step 3")
      (mq/send xmitter "READY\u0000")
      (.close xmitter))))

(defn -main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pair)]
    (mq/bind receiver "inproc://step3")
    (-> ctx Step2. Thread. .start)
    (mq/recv-str receiver)
    (.close receiver)
    (println "Test successful!")
    (.term ctx)))
