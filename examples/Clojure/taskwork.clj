(ns taskwork
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;
;; Task worker
;; Connects PULL socket to tcp://localhost:5557
;; Collects workloads from ventilator via that socket
;; Connects PUSH socket to tcp://localhost:5558
;; Sends results to sink via that socket
;;
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [cxt (mq/context 1)
        receiver (mq/socket cxt mq/pull)
        sender (mq/socket cxt mq/push)]
    (mq/connect receiver "tcp://localhost:5557")
    (mq/connect sender "tcp://localhost:5558")
    ;; Process tasks forever
    (while true
      (let [string (mq/recv-str receiver)
            msec (Long/parseLong string)]
        (print (str string "."))
        ;; Do the work
        (Thread/sleep msec)
        (mq/send sender "\u0000")))))
