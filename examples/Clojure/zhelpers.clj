(ns zhelpers
  (:refer-clojure :exclude [send])
  (:import [org.zeromq ZMQ ZMQ$Context ZMQ$Socket ZMQQueue])
  (:import [java.util Random])
  (:use [clojure.contrib.str-utils2 :only [trim]]))

(defn context [threads]
  (ZMQ/context threads))

(defmacro with-context
  [id threads & body]
  `(let [~id (context ~threads)]
     (try ~@body
          (finally (.term ~id)))))

(def sndmore ZMQ/SNDMORE)

(def router ZMQ/XREP)
(def dealer ZMQ/XREQ)
(def req ZMQ/REQ)
(def rep ZMQ/REP)
(def xreq ZMQ/XREQ)
(def xrep ZMQ/XREP)
(def pub ZMQ/PUB)
(def sub ZMQ/SUB)
(def pair ZMQ/PAIR)
(def push ZMQ/PUSH)
(def pull ZMQ/PULL)

(defn socket
  [#^ZMQ$Context context type]
  (.socket context type))

(defn queue
  [#^ZMQ$Context context #^ZMQ$Socket frontend #^ZMQ$Socket backend]
  (ZMQQueue. context frontend backend))

(defn bind
  [#^ZMQ$Socket socket url]
  (doto socket
    (.bind url)))

(defn connect
  [#^ZMQ$Socket socket url]
  (doto socket
    (.connect url)))

(defn subscribe
  ([#^ZMQ$Socket socket #^String topic]
     (doto socket
       (.subscribe (.getBytes topic))))
  ([#^ZMQ$Socket socket]
     (subscribe socket "")))

(defn unsubscribe
  ([#^ZMQ$Socket socket #^String topic]
     (doto socket
       (.unsubscribe (.getBytes topic))))
  ([#^ZMQ$Socket socket]
     (unsubscribe socket "")))

(defmulti send (fn [#^ZMQ$Socket socket message & flags]
                 (class message)))

(defmethod send String
  ([#^ZMQ$Socket socket #^String message flags]
     (.send socket (.getBytes message) flags))
  ([#^ZMQ$Socket socket #^String message]
     (send socket message ZMQ/NOBLOCK)))

(defn send-more [#^ZMQ$Socket socket message]
  (send socket message sndmore))

(defn identify
  [#^ZMQ$Socket socket #^String name]
  (.setIdentity socket (.getBytes name)))

(defn recv
  ([#^ZMQ$Socket socket flags]
     (.recv socket flags))
  ([#^ZMQ$Socket socket]
     (recv socket 0)))

(defn recv-all
  [#^ZMQ$Socket socket flags]
  (loop [msg (recv socket flags) acc '[]]
    (if (.hasReceiveMore socket)
      (recur (recv socket flags) (conj acc msg))
      (conj acc msg))))

(defn recv-str [#^ZMQ$Socket socket]
  (-> socket recv String. trim))

(defn dump
  [#^ZMQ$Socket socket]
  (println (->> "-" repeat (take 38) (apply str)))
  (doseq [msg (recv-all socket 0)]
    (print (format "[%03d] " (count msg)))
    (if (and (= 17 (count msg)) (= 0 (first msg)))
      (println (format "UUID %s" (-> msg String. trim)))
      (println (-> msg String. trim)))))

(defn set-id
  ([#^ZMQ$Socket socket #^int n]
    (let [rdn (Random. (System/currentTimeMillis))]
      (identify socket (str (.nextLong rdn) "-" (.nextLong rdn) n))))
  ([#^ZMQ$Socket socket]
     (set-id socket 0)))
