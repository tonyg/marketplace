#lang typed/racket/base

(require "../sugar-typed.rkt")

(provide generic-spy)

(: generic-spy : (All (ParentState) Any -> (Spawn ParentState)))
(define (generic-spy label)
  (name-process `(generic-spy ,label)
    (spawn: #:parent : ParentState
	    #:child : Void
	    (transition: (void) : Void
	      (observe-publishers: Void (wild)
		(match-orientation orientation
		  (match-conversation topic
		    (match-interest-type interest
		      (match-reason reason
			(on-presence (begin (write `(,label ENTER (,orientation ,topic ,interest)))
					    (newline)
					    (flush-output)
					    '()))
			(on-absence (begin (write `(,label EXIT (,orientation ,topic ,interest)))
					   (newline)
					   (display reason)
					   (newline)
					   (flush-output)
					   '()))
			(on-message
			 [p (begin (write `(,label MSG ,p))
				   (newline)
				   (flush-output)
				   '())]))))))))))
