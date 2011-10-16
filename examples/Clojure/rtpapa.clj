(ns rtpapa
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))
;;
;; Custom route Router to Papa (ROUTER to REP)
;; Isaiah Peng <issaria@gmail.com>
;;

;; We will do this all in one thread to emphasize the sequence
;; of events…

(defn -main []
  (let [ctx (mq/context 1)
        client (mq/socket ctx mq/router)
        worker (mq/socket ctx mq/rep)]
    (mq/bind client "ipc://routing.ipc")
    (mq/identify worker "A")
    (mq/connect worker "ipc://routing.ipc")
    ;; Wait for the worker to connect so that when we send a message
    ;; with routing envelope, it will actually match the worker…
    (Thread/sleep 1000)
    (mq/send-more client "A")
    (mq/send-more client "address 3")
    (mq/send-more client "address 2")
    (mq/send-more client "address 1")
    (mq/send-more client "")
    (mq/send client "This is the workload")
    ;; Worker should get just the workload
    (mq/dump worker)
    ;; We don't play with envelopes in the worker
    (mq/send worker "This is the reply")
    ;; Now dump what we got off the ROUTER socket...
    (mq/dump client)
    (.close client)
    (.close worker)
    (.term ctx)))
