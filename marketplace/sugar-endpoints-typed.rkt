#lang typed/racket/base

(require (for-syntax syntax/parse))
(require (for-syntax racket/base))
(require "support/dsl-typed.rkt")
(require (for-syntax "support/dsl-typed.rkt"))

(require racket/match)

(require (prefix-in core: "main.rkt"))

(provide name-endpoint
	 let-fresh
	 observe-subscribers:
	 observe-subscribers/everything:
	 observe-publishers:
	 observe-publishers/everything:
	 publish-on-topic:
	 subscribe-to-topic:
	 build-endpoint:)

(define&provide-dsl-helper-syntaxes "endpoint definition context"
  [match-state
   match-orientation
   match-conversation
   match-interest-type
   match-reason
   on-presence
   on-absence
   on-message])

;; Must handle:
;;  - orientation
;;  - interest-type
;;  - let-name
;;  - naming of endpoints
;;  - state matching
;;  - conversation (and generally role) matching
;;  - presence event handling
;;  - absence event handling (including reason matching)
;;  - message event handling (including message matching)

(: name-endpoint : (All (State) Any (core:AddEndpoint State) -> (core:AddEndpoint State)))
(define (name-endpoint n e)
  (match e
    [(core:add-endpoint _ role handler)
     (core:add-endpoint (cast n core:PreEID) role handler)]))

(define-syntax-rule (let-fresh (id ...) exp ...)
  (let ((id (gensym 'id)) ...) exp ...))

(define-syntax-rule (observe-subscribers: State topic clause ...)
  (build-endpoint: State
		   (gensym 'anonymous-endpoint)
		   (core:role 'publisher (cast topic core:Topic) 'observer)
		   clause ...))

(define-syntax-rule (observe-subscribers/everything: State topic clause ...)
  (build-endpoint: State
		   (gensym 'anonymous-endpoint)
		   (core:role 'publisher (cast topic core:Topic) 'everything)
		   clause ...))

(define-syntax-rule (observe-publishers: State topic clause ...)
  (build-endpoint: State
		   (gensym 'anonymous-endpoint)
		   (core:role 'subscriber (cast topic core:Topic) 'observer)
		   clause ...))

(define-syntax-rule (observe-publishers/everything: State topic clause ...)
  (build-endpoint: State
		   (gensym 'anonymous-endpoint)
		   (core:role 'subscriber (cast topic core:Topic) 'everything)
		   clause ...))

(define-syntax-rule (publish-on-topic: State topic clause ...)
  (build-endpoint: State
		   (gensym 'anonymous-endpoint)
		   (core:role 'publisher (cast topic core:Topic) 'participant)
		   clause ...))

(define-syntax-rule (subscribe-to-topic: State topic clause ...)
  (build-endpoint: State
		   (gensym 'anonymous-endpoint)
		   (core:role 'subscriber (cast topic core:Topic) 'participant)
		   clause ...))

(define-syntax build-endpoint:
  (lambda (stx)
    (define (combine-handler-clauses State
				     clauses-stx
				     stateful?
				     state-stx
				     orientation-stx
				     conversation-stx
				     interest-type-stx
				     reason-stx)

      (define (do-tail new-clauses-stx)
	(combine-handler-clauses State
				 new-clauses-stx
				 stateful?
				 state-stx
				 orientation-stx
				 conversation-stx
				 interest-type-stx
				 reason-stx))

      (define (stateful-lift context exprs-stx)
	(if stateful?
	    (syntax-case exprs-stx ()
	      [(expr)
	       #`(lambda: ([state : #,State]) (match state [#,state-stx expr]))]
	      [_
	       (raise-syntax-error #f
				   (format "Expected exactly one expression resulting in a transition, in ~a handler"
					   context)
				   stx
				   exprs-stx)])
	    (syntax-case exprs-stx ()
	      [(expr ...)
	       #`(lambda: ([state : #,State]) (core:transition state (list expr ...)))])))

      (syntax-case clauses-stx ()
	[() '()]

	[((maybe-match-state pat-stx inner-clause ...) outer-clause ...)
	 (literal-identifier=? #'maybe-match-state #'match-state)
	 (append (combine-handler-clauses State
					  (syntax (inner-clause ...))
					  #t
					  #'pat-stx
					  orientation-stx
					  conversation-stx
					  interest-type-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((maybe-match-orientation pat-stx inner-clause ...) outer-clause ...)
	 (literal-identifier=? #'maybe-match-orientation #'match-orientation)
	 (append (combine-handler-clauses State
					  (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  #'pat-stx
					  conversation-stx
					  interest-type-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((maybe-match-conversation pat-stx inner-clause ...) outer-clause ...)
	 (literal-identifier=? #'maybe-match-conversation #'match-conversation)
	 (append (combine-handler-clauses State
					  (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  orientation-stx
					  #'pat-stx
					  interest-type-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((maybe-match-interest-type pat-stx inner-clause ...) outer-clause ...)
	 (literal-identifier=? #'maybe-match-interest-type #'match-interest-type)
	 (append (combine-handler-clauses State
					  (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  orientation-stx
					  conversation-stx
					  #'pat-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((maybe-match-reason pat-stx inner-clause ...) outer-clause ...)
	 (literal-identifier=? #'maybe-match-reason #'match-reason)
	 (append (combine-handler-clauses State
					  (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  orientation-stx
					  conversation-stx
					  interest-type-stx
					  #'pat-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((maybe-on-presence expr ...) outer-clause ...)
	 (literal-identifier=? #'maybe-on-presence #'on-presence)
	 (cons #`[(core:presence-event (core:role #,orientation-stx
						  #,conversation-stx
						  #,interest-type-stx))
		  #,(stateful-lift 'on-presence (syntax (expr ...)))]
	       (do-tail (syntax (outer-clause ...))))]

	[((maybe-on-absence expr ...) outer-clause ...)
	 (literal-identifier=? #'maybe-on-absence #'on-absence)
	 (cons #`[(core:absence-event (core:role #,orientation-stx
						 #,conversation-stx
						 #,interest-type-stx)
				      #,reason-stx)
		  #,(stateful-lift 'on-absence (syntax (expr ...)))]
	       (do-tail (syntax (outer-clause ...))))]

	[((maybe-on-message [message-pat expr ...] ...) outer-clause ...)
	 (literal-identifier=? #'maybe-on-message #'on-message)
	 (cons #`[(core:message-event (core:role #,orientation-stx
						 #,conversation-stx
						 #,interest-type-stx)
				      message)
		  (match message
		    #,@(map (lambda (message-clause)
			      (syntax-case message-clause ()
				([message-pat expr ...]
				 #`[message-pat #,(stateful-lift 'on-message
								 (syntax (expr ...)))])))
			    (syntax->list (syntax ([message-pat expr ...] ...))))
		    [_ (lambda: ([state : #,State]) (core:transition state '()))])]
	       (do-tail (syntax (outer-clause ...))))]

	[(unknown-clause outer-clause ...)
	 (raise-syntax-error #f
			     "Illegal clause in endpoint definition"
			     stx
			     #'unknown-clause)]))

    (syntax-case stx ()
      [(dummy State pre-eid-exp role-exp handler-clause ...)
       #`(core:add-endpoint (cast pre-eid-exp core:PreEID)
			    role-exp
			    (match-lambda
			     #,@(reverse
				 (combine-handler-clauses
				  #'State
				  (syntax (handler-clause ...))
				  #f
				  (syntax old-state)
				  (syntax _)
				  (syntax _)
				  (syntax _)
				  (syntax _)))
			     [_ (lambda: ([state : State]) (core:transition state '()))]))])))

;;; Local Variables:
;;; eval: (put 'name-endpoint 'scheme-indent-function 1)
;;; eval: (put 'let-fresh 'scheme-indent-function 1)
;;; eval: (put 'observe-subscribers: 'scheme-indent-function 2)
;;; eval: (put 'observe-subscribers/everything: 'scheme-indent-function 2)
;;; eval: (put 'observe-publishers: 'scheme-indent-function 2)
;;; eval: (put 'observe-publishers/everything: 'scheme-indent-function 2)
;;; eval: (put 'publish-on-topic: 'scheme-indent-function 2)
;;; eval: (put 'subscribe-to-topic: 'scheme-indent-function 2)
;;; eval: (put 'match-state 'scheme-indent-function 1)
;;; eval: (put 'match-orientation 'scheme-indent-function 1)
;;; eval: (put 'match-conversation 'scheme-indent-function 1)
;;; eval: (put 'match-interest-type 'scheme-indent-function 1)
;;; eval: (put 'match-reason 'scheme-indent-function 1)
;;; End:
