(ns peering2
  (:refer-clojure :exclude [send])
  (:use [zhelpers :as mq]))
 
;;
;; Broker peering simulation (part 2)
;; Prototypes the request-reply flow
;;
;; While this example runs in a single process, that is just to make
;; it easier to start and stop the example. Each thread has its own
;; context and conceptually acts as a separate process.
;; Isaiah Peng <issaria@gmail.com>

(defn -main [& args])
