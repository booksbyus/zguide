(ns storm_demo.taskwork2
  (:refer-clojure :exclude [send])
  (:require [zilch.mq :as mq])
  (:import [org.zeromq ZMQ$Poller])
  (:use [clojure.contrib.str-utils2 :only [trim]]))

;
; Task worker - design 2 in Clojure
; Adds pub-sub flow to receive and respond to kill signal
; Isaiah Peng <issaria@gmail.com>
; 

(defn main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        sender (mq/socket ctx mq/push)
        controller (mq/socket ctx mq/sub)
        items (.poller ctx 2)]
    (mq/connect receiver "tcp://localhost:5557")
    (mq/connect sender "tcp://localhost:5558")
    (mq/connect controller "tcp://localhost:5559")
    (mq/subscribe controller (.getBytes ""))
    (.register items receiver ZMQ$Poller/POLLIN)
    (.register items controller ZMQ$Poller/POLLIN)
    ; Any waiting controller commands acts as 'KILL'
    (while (not (.pollin items 1))
      (if (.pollin items 0)
        (let [string (-> receiver mq/recv String. trim)
              nsec (* (Long/parseLong string) 1000000)]
          (print (str string "."))
          (Thread/sleep nsec)
          (mq/send (.getBytes "") 0)
          (println "."))))
    ; Finished
    (.close receiver)
    (.close sender)
    (.close controller)
    (.term ctx)))
