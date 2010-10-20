#lang racket
#|
#  Synchronized subscriber
|#
(require (planet jaymccarthy/zeromq))

(define ctxt (context 1))

; First, connect our subscriber socket
(define subscriber (socket ctxt 'SUB))
(socket-connect! subscriber "tcp://localhost:5561")
(set-socket-option! subscriber 'SUBSCRIBE #"")

; Second, synchronize with publisher
(define syncclient (socket ctxt 'REQ))
(socket-connect! syncclient "tcp://localhost:5562")
    
; send a synchronization request
(socket-send! syncclient #"")
    
; wait for synchronization reply
(void (socket-recv! syncclient))

; Third, get our updates and report how many we got
(define nbr
  (let loop ([nbr 0])
    (define msg (socket-recv! subscriber))
    (printf "Received: ~a\n" msg)
    (if (bytes=? msg #"END")
        nbr
        (loop (add1 nbr)))))

(printf "Received ~a updates\n" nbr)

(context-close! ctxt)