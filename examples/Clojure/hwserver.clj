;; Hello World server in Clojure
;; Binds REP socket to tcp://*:5555
;; Expects "Hello" from client, replies with "World"

(ns hwserver
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [context (zmq/context 1)]
    (with-open [socket (doto (zmq/socket context :rep)
                         (zmq/bind "tcp://*:5555"))]
      (while (not (.. Thread currentThread isInterrupted))
        (let [reply (zmq/receive socket)]
          (println "Received Hello")
          ;; Do some work
          (Thread/sleep 1000)
          (zmq/send-str socket "World"))))))
