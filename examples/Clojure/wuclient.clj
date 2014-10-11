;; Weather update client
;; Connects SUB socket to tcp://localhost:5556
;; Collects weather updates and finds avg temp in zipcode

(ns wuclient
  (:require [zeromq.zmq :as zmq]
            [clojure.string :as str]))

(defn message->temperature [socket]
  (-> (zmq/receive-str socket)
      (str/split #" ")
      (second)
      (#(Long/parseLong %))))

(defn -main [& args]
  (println "Collecting updates from weather server…")
  (let [filter (or (first args) "10001")
        context (zmq/zcontext)]
    (let [subscriber (doto (zmq/socket context :sub)
                       (zmq/connect "tcp://127.0.0.1:5556")
                       (zmq/subscribe filter))]
    (try
        (let [times 10
              temps (repeatedly times (partial message->temperature subscriber))
              avg (int (/ (apply + temps) (count temps)))]
          (printf "Average temperature for zipcode '%s' was %d\n" filter avg))
    (finally (.destroySocket context subscriber) ;; close hangs the process
            (.destroy context))))))
    
