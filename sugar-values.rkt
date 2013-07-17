#lang typed/racket/base

(require racket/match)
(require (prefix-in core: "main.rkt"))

(provide transition
	 delete-endpoint
	 send-message
	 send-feedback
	 quit
	 sequence-actions
	 (rename-out [core:wild wild]))

(: transition : (All (State) State (core:ActionTree State) * -> (core:Transition State)))
(define (transition state . actions)
  ((inst core:transition State) state actions))

(define (delete-endpoint #{id : Any}
			 [#{reason : Any} #f])
  (core:delete-endpoint (cast id core:PreEID) (cast reason core:Reason)))

(: send-message : (case-> [Any -> core:send-message]
			  [Any core:Orientation -> core:send-message]))
(define (send-message body [#{orientation : core:Orientation} 'publisher])
  (core:send-message (cast body core:Message) orientation))

(define (send-feedback #{body : Any})
  (core:send-message (cast body core:Message) 'subscriber))

(: quit : (case-> [-> core:quit]
		  [(Option core:PID) -> core:quit]
		  [(Option core:PID) Any -> core:quit]))
(define (quit [#{who : (Option core:PID)} (ann #f (Option core:PID))]
	      [#{reason : Any} #f])
  (core:quit who (cast reason core:Reason)))

(: sequence-actions : (All (State)
			   (core:Transition State)
			   (U (core:ActionTree State) (State -> (core:Transition State))) *
			   -> (core:Transition State)))
(define (sequence-actions t . more-actions-and-transformers)
  (match-define (core:transition initial-state initial-actions) t)
  (let loop ((state initial-state)
	     (actions initial-actions)
	     (items more-actions-and-transformers))
    (match items
      ['()
       (core:transition state actions)]
      [(cons item remaining-items)
       (if (or (pair? item)
	       (eq? item #f)
	       (void? item)
	       (null? item)
	       (core:add-endpoint? item)
	       (core:delete-endpoint? item)
	       (core:send-message? item)
	       (core:spawn? item)
	       (core:quit? item)
	       (core:yield? item)
	       (core:at-meta-level? item))
	   ;; ^ This is ugly, but necessary to let Typed Racket
	   ;; correctly deduce the type of item in the expression
	   ;; (item state) in the false branch of this conditional.
	   ;; Because the type Action is parameterized, there's no
	   ;; sensible way of factoring out the big or here into a
	   ;; reusable predicate.
	   (loop state
		 ((inst cons (core:ActionTree State) (core:ActionTree State))
		  actions
		  item)
		 remaining-items)
	   (match (item state)
	     [(core:transition new-state more-actions)
	      (loop new-state
		    (cons actions more-actions)
		    remaining-items)]))])))

;;; Local Variables:
;;; eval: (put 'sequence-actions 'scheme-indent-function 1)
;;; End:
