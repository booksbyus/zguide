#lang racket
#|
#   Broker peering simulation (part 1) in Racket
#   Prototypes the state flow
|#
(require (planet jaymccarthy/zeromq))

(define (main myself peers)
  (printf "Hello, I am ~a\n" myself)
  (define ctxt (context 1))

  ; State Back-End
  (define statebe (socket ctxt 'PUB))

  ; State Front-End
  (define statefe (socket ctxt 'SUB))
  (set-socket-option! statefe 'SUBSCRIBE #"")
  
  (define bind-address (format "ipc://~a-state.ipc" myself))
  (socket-bind! statebe bind-address)
  (for ([p (in-list peers)])
    (socket-connect! statefe (format "ipc://~a-state.ipc" p)))
  
  (define poller
    (vector (make-poll-item statefe 0 'POLLIN empty)))
  (let loop ()
    (poll! poller 1000000)
    
    (define revents (poll-item-revents (vector-ref poller 0)))
    (if (equal? revents '(POLLIN))
        (printf "Received: ~a" (socket-recv! statefe))
        (socket-send! statebe 
                      (string->bytes/utf-8
                       (format "~a ~a" bind-address (random 10)))))
    
    (loop))
  
  (context-close! ctxt))

(command-line #:program "peering1"
              #:args (myself . peers)
              (main myself peers))
