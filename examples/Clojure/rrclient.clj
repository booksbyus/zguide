(ns rrclient
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;                                             
;; Hello World client                           
;; Connects REQ socket to tcp://localhost:5559  
;; Sends "Hello" to server, expects "World" back
;;

(defn -main []
  (let [ctx (mq/context 1)
        requester (mq/socket ctx mq/req)]
    (mq/connect requester "tcp://localhost:5559")
    (dotimes [i 10]
      (mq/send requester "Hello\u0000")
      (let [string (mq/recv-str requester)]
        (println (format "Received reply %d %s" i string))))
    (.close requester)
    (.term ctx)))

