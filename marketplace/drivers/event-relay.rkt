#lang typed/racket/base
;; Ground-event relay.

(provide event-relay)
(require "../sugar-typed.rkt")
(require "../support/event.rkt")

(: event-relay : (All (ParentState) Symbol -> (Spawn ParentState)))
(define (event-relay self-id)
  (name-process `(event-relay ,self-id)
    (spawn: #:parent : ParentState
	    #:child : Void
	    (transition/no-state
	      (observe-publishers: Void (cons ? ?)
		(match-conversation (cons (? evt? e) _)
		  (on-presence (begin
				 (printf "SUBSCRIBED ~v~n" e)
				 (flush-output)
				 (at-meta-level: Void
				   (name-endpoint `(event-relay ,self-id ,e)
				     (subscriber: Void (cons e ?)
				       (on-message
					[msg (begin (printf "FIRED ~v -> ~v~n" e msg)
						    (flush-output)
						    (send-message msg))]))))))
		  (on-absence (begin
				(printf "UNSUBSCRIBED ~v~n" e)
				(flush-output)
				(at-meta-level: Void
				  (delete-endpoint `(event-relay ,self-id ,e)))))))))))
