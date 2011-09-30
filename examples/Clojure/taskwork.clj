(ns storm_demo.taskwork
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq])
  (:use [clojure.contrib.str-utils2 :only [trim]]))

;
; Task worker in Clojure
; Isaiah Peng <issaria@gmail.com>
;

(defn main []
  (let [cxt (mq/context 1)
        receiver (mq/socket cxt mq/pull)
        sender (mq/socket cxt mq/push)]
    (mq/connect receiver "tcp://localhost:5557")
    (mq/connect sender "tcp://localhost:5558")
    ; Process tasks forever
    (loop []
      (let [string (-> receiver (mq/recv 0) String. trim)
            msec (Long/parseLong string)]
        (print (str string "."))
        ; Do the work
        (Thread/sleep msec)
        (mq/send sender (.getBytes "\u0000") 0)))))
