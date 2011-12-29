(ns rtdealer
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [java.util Random]))

;;
;; Custom routing Router to Dealer
;;
;; While this example runs in a single process, that is just to make
;; it easier to start and stop the example. Each thread has its own
;; context and conceptually acts as a separate process.
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defrecord Worker [name]
  Runnable
  (run [this]
    (let [ctx (mq/context 1)
          worker (mq/socket ctx mq/dealer)]
      (mq/identify worker name)
      (mq/connect worker "ipc://routing.ipc")
      (loop [total 0]
        (let [request (mq/recv-str worker)]
          (if (= "END" request)
            (println (format "%s received: %d" name total))
            (recur (inc total))))))))

(defn -main []
  (let [ctx (mq/context 1)
        client (mq/socket ctx mq/dealer)
        srandom (Random. (System/currentTimeMillis))]
    (mq/bind client "ipc://routing.ipc")
    (-> "A" Worker. Thread. .start)
    (-> "B" Worker. Thread. .start)
    ;; Wait for threads to connect, since otherwise the messages
    ;; we send won't be routable.
    (Thread/sleep 1000)
    ;; Send 10 tasks scattered to A twice as often as B
    (dotimes [i 10]
      ;; Send two message parts, first the address...
      (if (= 0 (.nextInt srandom 3))
        (mq/send-more client "B")
        (mq/send-more client "A"))
      ;; And then the workload
      (mq/send client "This is the workload"))
    (mq/send-more client "A")
    (mq/send client "END")
    (mq/send-more client "B")
    (mq/send client "END")
    (.close client)
    (.term ctx)))
