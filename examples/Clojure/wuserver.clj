(ns storm_demo.wuserver
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq])
  (:import [java.util Random]))

(defn main []
  (let [publisher (-> 1 mq/context (mq/socket mq/pub))
        srandom (Random. (System/currentTimeMillis))]
    (mq/bind publisher "tcp://*:5556")
    (mq/bind publisher "ipc://weather")
    (loop []
      (let [zipcode (-> srandom (.nextInt 100000) (+ 1))
              temperature (-> srandom (.nextInt 215) (- 79))
              relhumidity (-> srandom (.nextInt 50) (+ 11))
              update (format "%05d %d %d\u0000" zipcode temperature relhumidity)]
        ; Send message to all subscribers
        (mq/send publisher (.getBytes update) 0))
      (recur))))
