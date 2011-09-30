;
;  Hello World client in Clojure
;  Connects REQ socket to tcp://localhost:5555
;  Sends "Hello" to server, expects "World" back
;
;  Isaiah Peng <issarai@gmail.com>
;

(ns examples.hwclient
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq]))

(defn run []
  (let [sock (-> 1 mq/context (mq/socket mq/req))]
    (mq/connect sock "tcp://localhost:5555")
    (for [i (range 10)]
      (let [request (.getBytes "Hello ")]
        (aset-byte request (-> request count (- 1)) 0)
        (println "Sending request...")
        (mq/send sock request 0)
        (let [reply (mq/recv sock 0)]
          (println (str "Received reply " i ":" (String. reply))))))))
