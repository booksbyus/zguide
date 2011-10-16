(ns rtmama
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [java.util Random]))

;;
;; Custom routing Router to Mama (ROUTER to REQ)
;;
;; While this example runs in a single process, that is just to make
;; it easier to start and stop the example. Each thread has its own
;; context and conceptually acts as a separate process.
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defrecord Worker [n]
  Runnable
  (run [this]
    (let [ctx (mq/context 1)
          worker (mq/socket ctx mq/req)
          srandom (Random. (System/currentTimeMillis))]
      (mq/set-id worker n)
      (mq/connect worker "ipc://routing.ipc")
      (loop [total 0]
        (let [_ (mq/send worker "ready") workload (mq/recv-str worker)]
          ;; Do some random work
          (Thread/sleep (+ (.nextInt srandom 1000) 1000))
          (if (not= "END" workload)
            (recur (inc total))
            (println (format "Worker %d processed %d tasks" n total)))))
      (.close worker)
      (.term ctx))))

(def worker-nbr 10)
(defn -main []
  (let [ctx (mq/context 1)
        client (mq/socket ctx mq/router)
        srandom (Random. (System/currentTimeMillis))]
    (mq/bind client "ipc://routing.ipc")
    (dotimes [i worker-nbr]
      (-> i Worker. Thread. .start))
    (dotimes [i (* 10 worker-nbr)]
      ;; LRU worker is next waiting in queue
      (let [address (mq/recv-str client) _ (mq/recv client) _ (mq/recv client)]
        (mq/send-more client address)
        (mq/send-more client "")
        (mq/send client "This is the workload")))
    ;; Now ask mamas to shutdown and report their results
    (dotimes [i worker-nbr]
      (let [address (mq/recv-str client) _ (mq/recv client) _ (mq/recv client)]
        (mq/send-more client address)
        (mq/send-more client "")
        (mq/send client "END")))
    (.close client)
    (.term ctx)))
