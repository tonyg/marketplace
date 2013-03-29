#lang typed/racket/base

(require "../sugar-typed.rkt")

(provide generic-spy)

(: generic-spy : (All (ParentState) Any -> (Spawn ParentState)))
(define (generic-spy label)
  (spawn: #:debug-name `(generic-spy ,label)
	  #:parent : ParentState
	  #:child : Void
	  (transition: (void) : Void
	    (endpoint: : Void
		       #:subscriber (wild) #:observer
		       #:peer-orientation orientation
		       #:conversation topic
		       #:peer-interest-type interest
		       #:reason reason
		       #:on-presence (begin (write `(,label ENTER (,orientation ,topic ,interest)))
					    (newline)
					    (flush-output)
					    '())
		       #:on-absence (begin (write `(,label EXIT (,orientation ,topic ,interest)))
					   (newline)
					   (display reason)
					   (newline)
					   (flush-output)
					   '())
		       [p (begin (write `(,label MSG ,p))
				 (newline)
				 (flush-output)
				 '())]))))
