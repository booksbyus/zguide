(ns lruqueue
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [org.zeromq ZMQ$Poller]))

;;                                                   
;; Least-recently used (LRU) queue device              
;; Clients and workers are shown here in-process       
;;                                                     
;; While this example runs in a single process, that is just to make
;; it easier to start and stop the example. Each thread has its own
;; context and conceptually acts as a separate process.
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defrecord Client [n]
  Runnable
  (run [this]
    (let [cntx (mq/context 1)
          client (mq/socket cntx mq/req)]
      (mq/set-id client n)
      (mq/connect client "ipc://frontend.ipc")
      (mq/send client "HELLO")
      (println (format "Client: %s" (mq/recv-str client)))
      (.close client)
      (.term cntx))))

(defrecord Worker [n]
  Runnable
  (run [this]
    (let [cnt (mq/context 1)
          worker (mq/socket cnt mq/req)]
      (mq/set-id worker n)
      (mq/connect worker "ipc://backend.ipc")
      (mq/send worker "READY")
      (while true
        (let [address (mq/recv-str worker)
              _ (mq/recv worker)
              request (mq/recv-str worker)]
          (println (format "Worker: %s" request))
          (mq/send-more worker address)
          (mq/send-more worker "")
          (mq/send worker "OK")))
      (.close worker)
      (.term cnt))))

(def worker-nbr 3)
(def client-nbr 10)

(defn -main []
  (let [ctx (mq/context 1)
        frontend (mq/socket ctx mq/router)
        backend (mq/socket ctx mq/router)]
    (mq/bind frontend "ipc://frontend.ipc")
    (mq/bind backend "ipc://backend.ipc")
    (dotimes [i client-nbr]
      (-> i Client. Thread. .start))
    (dotimes [i worker-nbr]
      (-> i Worker. Thread. .start)
      ;; Logic of LRU loop
      ;; - Poll backend always, frontend only if 1+ worker ready
      ;; - If worker replies, queue worker as ready and forward reply
      ;; to client if necessary
      ;; - If client requests, pop next worker and send request to it
      ;;
      ;; Queue of available workers
      (let [available-workers (atom 0)
            worker-queue (atom clojure.lang.PersistentQueue/EMPTY)
            items (.poller ctx 2)]
        (while (not (.isInterrupted (Thread/currentThread)))
          (dotimes [i client-nbr]
            (.register items backend ZMQ$Poller/POLLIN)
            (.register items frontend ZMQ$Poller/POLLIN)
            (.poll items)
            ;; Handle worker activity on background
            (if (.pollin items 0)
              ;; Queue worker address for LRU routing
              (swap! worker-queue #(conj % (mq/recv-str backend)))
              (let [_ (mq/recv backend) client-addr (mq/recv-str backend)]
                (if (not= "READY" client-addr)
                  (let [_ (mq/recv backend) reply (mq/recv-str backend)]
                    (mq/send-more frontend client-addr)
                    (mq/send-more frontend "")
                    (mq/send frontend reply)))))
            (if (.pollin items 1)
              (let [client-addr (mq/recv-str frontend)
                    _ (mq/recv frontend)
                    request (mq/recv-str frontend)
                    worker-addr (peek @worker-queue)]
                (swap! worker-queue pop)
                (mq/send-more backend worker-addr)
                (mq/send-more backend "")
                (mq/send-more backend client-addr)
                (mq/send-more backend "")
                (mq/send backend request)))))
        (.close frontend)
        (.close backend)
        (.term ctx)))))
