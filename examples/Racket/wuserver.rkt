#lang racket
#|
#   Weather update server
#   Binds PUB socket to tcp://*:5556
#   Publishes random weather updates
|#
(require (planet jaymccarthy/zeromq))

(define ctxt (context 1))
(define sock (socket ctxt 'PUB))
(socket-bind! sock "tcp://*:5556")

(let loop ()
  (define zipcode (random 100000))
  (define temp (- (random 215) 80))
  (define humidity (+ (random 50) 10))
  
  (socket-send! sock (string->bytes/utf-8 (format "~a ~a ~a" zipcode temp humidity)))
  
  (loop))

(context-close! ctxt)