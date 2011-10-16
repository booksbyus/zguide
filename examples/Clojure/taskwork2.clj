(ns taskwork2
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq])
  (:import [org.zeromq ZMQ$Poller]))

;;
;; Task worker - design 2
;; Adds pub-sub flow to receive and respond to kill signal
;;
;; Isaiah Peng <issaria@gmail.com>
;; 

(defn -main []
  (let [ctx (mq/context 1)
        receiver (mq/socket ctx mq/pull)
        sender (mq/socket ctx mq/push)
        controller (mq/socket ctx mq/sub)
        items (.poller ctx 2)
        continue (atom true)]
    (mq/connect receiver "tcp://localhost:5557")
    (mq/connect sender "tcp://localhost:5558")
    (mq/connect controller "tcp://localhost:5559")
    (mq/subscribe controller "")
    (.register items receiver ZMQ$Poller/POLLIN)
    (.register items controller ZMQ$Poller/POLLIN)
    ;; Any waiting controller commands acts as 'KILL'
    (while @continue ; Any better idea to break the loop?
      (.poll items)
      (if (.pollin items 0)
        (let [string (mq/recv-str receiver)
              nsec (Long/parseLong string)]
          ;; Do the work
          (Thread/sleep nsec)
          ;; Send results to sink
          (mq/send sender string)
          ;; Simple progress indicator for the viewer
          (print ".")))
      (if (.pollin items 1)
        (reset! continue false)))
    ;; Finished
    (.close receiver)
    (.close sender)
    (.close controller)
    (.term ctx)))
