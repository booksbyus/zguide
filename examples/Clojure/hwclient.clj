;;
;;  Hello World client in Clojure
;;  Connects REQ socket to tcp://localhost:5555
;;  Sends "Hello" to server, expects "World" back
;;
;;  Isaiah Peng <issarai@gmail.com>
;;

(ns hwclient
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

(defn -main []
  (let [sock (-> 1 mq/context (mq/socket mq/req))]
    (mq/connect sock "tcp://localhost:5555")
    (dotimes [i 10]
      (println "Sending request...")
      (mq/send sock "Hello\u0000")
      (let [reply (mq/recv-str sock)]
        (println (str "Received reply " i ":" reply))))))
