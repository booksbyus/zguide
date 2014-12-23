(ns lpclient
  (:require [zeromq.zmq :as zmq]
            [clojure.core.match :refer [match]]))

(def request-timeout 2500)
(def request-retries 3)
(def server-endpoint "tcp://localhost:5555")

(defn new-client [ctx]
  (println "I: connecting to server...")
  (doto (zmq/socket ctx :req)
    (zmq/connect server-endpoint)))

(defn try-read [ctx client]
  (let [poller (zmq/poller ctx 1)
        client-poll-id (zmq/register poller client :pollin)]
    (if (= -1 (zmq/poll poller request-timeout))
      [:error]
      (if (zmq/check-poller poller client-poll-id :pollin)
        [:resp (zmq/receive-str client)]
        [:timeout]))))

(defn retry-send [ctx client value request-retries]
  (zmq/send-str client value)
  (match (try-read ctx client)
         [:error] (recur ctx client value request-retries)
         [:resp resp]
         (cond
          (nil? resp) (recur ctx client value request-retries)
          (= resp value) (do
                           (println (format "I: server replied OK (%s)" value))
                           client)
          :else (do (println (format "E: malformed reply from server: %s" value))
                    nil))
         [:timeout] (if (> request-retries 0)
                      (do (println "W: no response from server, retrying...")
                          (recur ctx (new-client ctx) value (dec request-retries)))
                      (do (println "E: server seems to be offline, abandoning")
                          nil))))

(defn -main []
  (let [ctx (zmq/zcontext)]
    (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (zmq/destroy ctx))))
    (loop [client (new-client ctx)
           sequence 0]
      (when-let [client (retry-send ctx client (str sequence) request-retries)]
        (recur client (inc sequence))))))
