;; Weather update server
;; Binds PUB socket to tcp://*:5556
;; Publishes random weather updates

(ns wuserver
  (:require [zeromq.zmq :as zmq]))

(defn -main []
  (let [context (zmq/zcontext)]
    (with-open [publisher (doto (zmq/socket context :pub)
                            (zmq/bind "tcp://*:5556")
                            (zmq/bind "ipc://weather.ipc"))]
      (while (not (.. Thread currentThread isInterrupted))
        (let [zipcode (rand-int 100000)
              temperature (- (rand-int 215) 80)
              relhumidity (+ (rand-int 50) 10)]
          (zmq/send-str publisher
                        (format "%05d %d %d" zipcode temperature relhumidity)))))))
