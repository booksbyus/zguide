(ns identity
  (:refer-clojure :exclude [send])
  (:require [zhelpers :as mq]))

;;                                                                   
;; Demonstrate identities as used by the request-reply pattern. Run this
;; program by itself. Note that the utility functions s_ are provided by
;; zhelpers.h. It gets boring for everyone to keep repeating this code.
;; Isaiah Peng <issaria@gmail.com>
;;

(defn -main []
  (let [ctx (mq/context 1)
        sink (mq/socket ctx mq/router)
        anonymous (mq/socket ctx mq/req)
        identified (mq/socket ctx mq/req)]
    (mq/bind sink "inproc://example")
    (mq/connect anonymous "inproc://example")
    (mq/send anonymous "ROUTER uses a generated UUID\u0000")
    (mq/dump sink)
    (mq/identify identified "Hello")
    (mq/connect identified "inproc://example")
    (mq/send identified "ROUTER socket uses REQ's socket identity\u0000")
    (mq/dump sink)
    (.close sink)
    (.close anonymous)
    (.close identified)
    (.term ctx)))
