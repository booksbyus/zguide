;;
;;  Hello World server in Clojure
;;  Binds REP socket to tcp://*:5555
;;  Expects "Hello" from client, replies with "World"
;;
;;  Isaiah Peng <issaria@gmail.com>
;;

(ns hwserver
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

(defn -main []
  (let [sock (-> 1 mq/context (mq/socket mq/rep))]
    (mq/bind sock "tcp://*:5555")
    (while true
      (let [req (mq/recv-str sock)
            reply "World\u0000"]
        (println (str "Received request: [ " req " ]"))
        ;; Do some 'work'
        (Thread/sleep 1000)

        ;;  Send reply back to client
        ;;  We will send a 0-terminated string (C string) back to the client,
        ;;  so that this server also works with The Guide's C and C++ "Hello World" clients
        (mq/send sock reply))
      )))
