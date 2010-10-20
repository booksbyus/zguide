#lang racket
#|
#  Synchronized publisher
|#
(require (planet jaymccarthy/zeromq))

;  We wait for 2 subscribers
(define SUBSCRIBERS_EXPECTED 2)

(define ctxt (context 1))

; Socket to talk to clients
(define publisher (socket ctxt 'PUB))
(socket-bind! publisher "tcp://*:5561")

; Socket to receive signals
(define syncservice (socket ctxt 'REP))
(socket-bind! syncservice "tcp://*:5562")

; Get synchronization from subscribers
(for ([i (in-range SUBSCRIBERS_EXPECTED)])
  ; wait for synchronization request
  (socket-recv! syncservice)
  ; send synchronization reply
  (socket-send! syncservice #"")
  (printf "+1 subscriber\n"))

; Now broadcast exactly 1M updates followed by END
(for ([i (in-range 1000)])
  (socket-send! publisher #"Rhubarb"))

(socket-send! publisher #"END")

(context-close! ctxt)