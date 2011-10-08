(ns examples.rtmama
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]])
  (:import [java.util Random]))

;
; Custom routing Router to Mama (ROUTER to REQ)
;
; While this example runs in a single process, that is just to make
; it easier to start and stop the example. Each thread has its own
; context and conceptually acts as a separate process.
;
; Isaiah Peng <issaria@gmail.com>
;

(defrecord Worker [n]
  Runnable
  (run [this]
    (let [ctx (mq/context 1)
          worker (mq/socket ctx mq/req)
          srandom (Random. (System/currentTimeMillis))]
      (mq/set-id worker n)
      (mq/connect worker "ipc://routing.ipc")
      (loop [_ (mq/send worker "ready") workload (-> worker mq/recv String. trim) total 0]
        (do
          ; Do some random work
          (Thread/sleep (+ (.nextInt srandom 1000) 1000))
          (if (not= "END" workload)
            (recur (mq/send worker "ready") (-> worker mq/recv String. trim) (+ total 1))
            (println (format "Worker %d processed %d tasks" n total)))))
      (.close worker)
      (.term ctx))))

(def worker-nbr 10)
(defn -main [& args]
  (let [ctx (mq/context 1)
        client (mq/socket ctx mq/router)
        srandom (Random. (System/currentTimeMillis))]
    (mq/bind client "ipc://routing.ipc")
    (doseq [i (range worker-nbr)]
      (-> (Worker. i) Thread. .start))
    (doseq [i (range (* 10 worker-nbr))]
      (let [address (-> client mq/recv String. trim) _ (mq/recv client) _ (mq/recv client)]
        (mq/send client address mq/sndmore)
        (mq/send client "" mq/sndmore)
        (mq/send client "This is the workload")))
    ; Now ask mamas to shutdown and report their results
    (doseq [i (range worker-nbr)]
      (let [address (-> client mq/recv String. trim) _ (mq/recv client) _ (mq/recv client)]
        (mq/send client address mq/sndmore)
        (mq/send client "" mq/sndmore)
        (mq/send client "END")))
    (.close client)
    (.term ctx)))
