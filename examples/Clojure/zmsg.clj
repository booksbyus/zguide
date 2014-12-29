(ns zmsg
  (:require [zeromq.zmq :as zmq]))

(defn frames->msg [frames]
  (->> frames
       (partition-by empty?)
       (remove (comp empty? first))
       (map (fn [f] (if (= (count f) 1) (first f) f)))
       flatten))

(defn msg->frames [msg-frames]
  (interpose (byte-array 0) msg-frames))

(def msg-id first)
(def msg-data last)
(def msg-unwrap rest)
(def msg-wrap cons)
(defn msg-replace-data [msg new-data]
  (concat (butlast msg) [new-data]))

(defn receive-msg [socket]
  (frames->msg (zmq/receive-all socket)))

(defn send-msg [socket msg]
  (let [frames (msg->frames msg)]
    (doseq [frame (butlast frames)]
      (zmq/send socket frame zmq/send-more))
    (zmq/send socket (last frames))))
