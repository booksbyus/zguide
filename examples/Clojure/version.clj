; Report 0MQ version

(ns version (:import [org.zeromq ZMQ]))

(defn -main []
  (println
   (format "Version string: %s, Version int: %d"
           (ZMQ/getVersionString)
           (ZMQ/getFullVersion))))
