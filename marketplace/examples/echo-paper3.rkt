#lang marketplace

(endpoint
 #:subscriber (tcp-channel ? (tcp-listener 5999) ?)
 #:conversation (tcp-channel from to _)
 #:on-presence (spawn #:child (echoer from to)))

(define (echoer from to)
  (transition stateless
    (endpoint
      #:subscriber (tcp-channel from to ?)
      #:on-absence (quit)
      [(tcp-channel _ _ data)
       (send-message (tcp-channel to from data))])))
