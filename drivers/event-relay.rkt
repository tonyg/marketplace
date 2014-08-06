#lang racket/base
;; Ground-event relay.

(provide event-relay)
(require "../sugar.rkt")

;; event-relay : (All (ParentState) Symbol -> (Spawn ParentState))
(define (event-relay self-id)
  (name-process `(event-relay ,self-id)
    (spawn (transition/no-state
	     (observe-subscribers (cons ? ?)
	       (match-conversation (cons (? evt? e) _)
		 (on-presence (begin
				(printf "SUBSCRIBED ~v~n" e)
				(flush-output)
				(at-meta-level
				 (name-endpoint `(event-relay ,self-id ,e)
				   (subscriber (cons e ?)
				     (on-message
				      [msg (begin (printf "FIRED ~v -> ~v~n" e msg)
						  (flush-output)
						  (send-message msg))]))))))
		 (on-absence (begin
			       (printf "UNSUBSCRIBED ~v~n" e)
			       (flush-output)
			       (at-meta-level
				(delete-endpoint `(event-relay ,self-id ,e)))))))))))
