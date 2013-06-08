#lang marketplace

(observe-publishers (tcp-channel ? (tcp-listener 5999) ?)
  (match-conversation (tcp-channel from to _)
    (on-presence (spawn (echoer from to)))))

(define (echoer from to)
  (transition stateless
    (publisher (tcp-channel to from ?))
    (subscriber (tcp-channel from to ?)
      (on-absence (quit))
      (on-message
       [(tcp-channel _ _ data)
	(send-message (tcp-channel to from data))]))))
