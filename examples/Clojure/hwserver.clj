;
;  Hello World server in Clojure
;  Binds REP socket to tcp://*:5555
;  Expects "Hello" from client, replies with "World"
;
;  Isaiah Peng <issaria@gmail.com>
;

(ns examples.hwserver
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq]))

(defn run []
  (let [sock (-> 1 mq/context (mq/socket mq/rep))]
    (mq/bind sock "tcp://*:5555")
    (loop []
      (let [req (recv sock)
            reply (.getBytes "World ")]
        (println (str "Received request: [" (String. req 0 (-> req count (- 1)) "]")))
        ; Do some 'work'
        (Thread/sleep 1)

        ;  Send reply back to client
        ;  We will send a 0-terminated string (C string) back to the client,
        ;  so that this server also works with The Guide's C and C++ "Hello World" clients
        (aset-byte reply (-> req count (- 1)) 0) ; Sets the last byte of the reply to 0
        (mq/send sock reply 0))
      (recur))))
