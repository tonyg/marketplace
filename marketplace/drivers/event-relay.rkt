#lang typed/racket/base
;; Ground-event relay.

(provide event-relay)
(require "../sugar-typed.rkt")
(require "../support/event.rkt")

(: event-relay : (All (ParentState) Symbol -> (Spawn ParentState)))
(define (event-relay self-id)
  (spawn: #:debug-name `(event-relay ,self-id)
	  #:parent : ParentState
	  #:child : Void
	  (transition/no-state
	    (endpoint: : Void
		       #:publisher (cons ? ?) #:observer
		       #:conversation (cons (? evt? e) _)
		       #:on-presence (begin
				       (printf "SUBSCRIBED ~v~n" e)
				       (flush-output)
				       (at-meta-level
					(endpoint: : Void
						   #:subscriber (cons e ?)
						   #:name `(event-relay ,self-id ,e)
						   [msg
						    (begin
						      (printf "FIRED ~v -> ~v~n" e msg)
						      (flush-output)
						      (send-message msg))])))
		       #:on-absence (begin
				      (printf "UNSUBSCRIBED ~v~n" e)
				      (flush-output)
				      (at-meta-level
				       (delete-endpoint `(event-relay ,self-id ,e))))))))
