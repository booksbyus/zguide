(ns examples.rtdealer
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]])
  (:import [java.util Random]))

;
; Custom routing Router to Dealer (ROUTER to DEALER)
;
; While this example runs in a single process, that is just to make
; it easier to start and stop the example. Each thread has its own
; context and conceptually acts as a separate process.
;
; Isaiah Peng <issaria@gmail.com>
;

(defrecord Worker [name]
  Runnable
  (run [this]
    (let [ctx (mq/context 1)
          worker (mq/socket ctx mq/dealer)]
      (mq/identify worker name)
      (mq/connect worker "ipc://routing.ipc")
      (loop [request (-> worker mq/recv String. trim) total 0]
        (if (= "END" request)
          (println (str name (format " received: %d" total)))
          (recur (-> worker mq/recv String. trim) (+ total 1)))))))

(defn -main [& args]
  (let [ctx (mq/context 1)
        client (mq/socket ctx mq/xrep)
        srandom (Random. (System/currentTimeMillis))]
    (mq/bind client "ipc://routing.ipc")
    (-> "A" Worker. Thread. .start)
    (-> "B" Worker. Thread. .start)
    ; Wait for threads to connect, since otherwise the messages
    ; we send won't be routable.
    (Thread/sleep 1000)
    (doseq [i (range 10)]
      (if (= 0 (mod (.nextInt srandom) 3))
        (mq/send client "B" mq/sndmore)
        (mq/send client "A" mq/sndmore))
      (mq/send client "This is the workload"))
    (mq/send client "A" mq/sndmore)
    (mq/send client "END")
    (mq/send client "B" mq/sndmore)
    (mq/send client "END")
    (.close client)
    (.term ctx)))
