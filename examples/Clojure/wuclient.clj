(ns storm_demo.wuclient
  (:refer-clojure :exclude [send])
  (:use [zilch.mq :as mq])
  (:require [clojure.contrib.str-utils2 :as str])
  (:import [java.util StringTokenizer]))

;
; Weather update client in Clojure
; Isaiah Peng <issaria@gmail.com>
;

(defn main [zipcodes*]
  (let [subscriber (-> 1 mq/context (mq/socket mq/sub))
        filter "10001 "
        total-temp (atom 0)
        nbr 100]
    (println "Collecting updates from weather server...")
    (mq/connect subscriber "tcp://localhost:5556")
    (mq/subscribe subscriber (.getBytes filter))
    (doseq [i (range nbr)]
      (let [string (-> subscriber (mq/recv 0) String. str/trim)
            sscanf (StringTokenizer. string " ")
            zipcode (Integer/parseInt (.nextToken sscanf))
            temperature (Integer/parseInt (.nextToken sscanf))
            relhumidity (Integer/parseInt (.nextToken sscanf))]
        (swap! total-temp #(+ % temperature))))
    (println (str "Average temperature for zipcode '" filter "' was " (int (/ @total-temp nbr))))))
