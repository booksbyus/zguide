#lang racket
#|
#   Weather update client
#   Connects SUB socket to tcp://localhost:5556
#   Collects weather updates and finds avg temp in zipcode
|#
(require (planet jaymccarthy/zeromq))

;  Socket to talk to server
(define ctxt (context 1))
(define sock (socket ctxt 'SUB))

(printf "Collecting updates from weather server...\n")
(socket-connect! sock "tcp://localhost:5556")

; Subscribe to zipcode, default is NYC, 10001
(define filter 
  (command-line #:program "wuclient" #:args maybe-zip 
                (match maybe-zip
                  [(list zipcode) zipcode]
                  [(list) "10001"]
                  [else (error 'wuclient "Incorrect argument list")])))
(set-socket-option! sock 'SUBSCRIBE (string->bytes/utf-8 filter))

; Process 5 updates
(define how-many 5)
(define total-temp
  (for/fold ([tot 0])
    ([i (in-range how-many)])
    (match-define (regexp #rx"([0-9]+) (-?[0-9]+) ([0-9]+)" (list _ zipcode temp humid))
                  (socket-recv! sock))
    (+ tot (string->number (bytes->string/utf-8 temp)))))

(printf "Average temperature for zipcode '~a' was ~a\n"
        filter (/ total-temp how-many))

(context-close! ctxt)