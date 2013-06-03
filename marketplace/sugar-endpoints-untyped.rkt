#lang racket/base

(require (for-syntax syntax/parse))
(require (for-syntax racket/base))
(require "support/dsl-untyped.rkt")

(require racket/match)

(require (prefix-in core: "main.rkt"))

(provide name-endpoint
	 let-fresh
	 observe-subscribers
	 observe-subscribers/everything
	 observe-publishers
	 observe-publishers/everything
	 publish-on-topic
	 subscribe-to-topic
	 build-endpoint)

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

(define (name-endpoint n e)
  (match e
    [(core:add-endpoint _ role handler)
     (core:add-endpoint n role handler)]))

(define-syntax-rule (let-fresh (id ...) exp ...)
  (let ((id (gensym 'id)) ...) exp ...))

(define-syntax-rule (observe-subscribers topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'publisher topic 'observer)
		  clause ...))

(define-syntax-rule (observe-subscribers/everything topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'publisher topic 'everything)
		  clause ...))

(define-syntax-rule (observe-publishers topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'subscriber topic 'observer)
		  clause ...))

(define-syntax-rule (observe-publishers/everything topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'subscriber topic 'everything)
		  clause ...))

(define-syntax-rule (publish-on-topic topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'publisher topic 'participant)
		  clause ...))

(define-syntax-rule (subscribe-to-topic topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'subscriber topic 'participant)
		  clause ...))

(define-syntax build-endpoint
  (lambda (stx)
    (define (combine-handler-clauses clauses-stx
				     stateful?
				     state-stx
				     orientation-stx
				     conversation-stx
				     interest-type-stx
				     reason-stx)

      (define (do-tail new-clauses-stx)
	(combine-handler-clauses new-clauses-stx
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
	       #`(match-lambda [#,state-stx expr])]
	      [_
	       (raise-syntax-error #f
				   (format "Expected exactly one expression resulting in a transition, in ~a handler"
					   context)
				   stx
				   exprs-stx)])
	    (syntax-case exprs-stx ()
	      [(expr ...)
	       #`(lambda (state) (core:transition state (list expr ...)))])))

      (syntax-case clauses-stx (match-state
				match-orientation
				match-conversation
				match-interest-type
				match-reason
				on-presence
				on-absence
				on-message)
	[() '()]

	[((match-state pat-stx inner-clause ...) outer-clause ...)
	 (append (combine-handler-clauses (syntax (inner-clause ...))
					  #t
					  #'pat-stx
					  orientation-stx
					  conversation-stx
					  interest-type-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((match-orientation pat-stx inner-clause ...) outer-clause ...)
	 (append (combine-handler-clauses (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  #'pat-stx
					  conversation-stx
					  interest-type-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((match-conversation pat-stx inner-clause ...) outer-clause ...)
	 (append (combine-handler-clauses (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  orientation-stx
					  #'pat-stx
					  interest-type-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((match-interest-type pat-stx inner-clause ...) outer-clause ...)
	 (append (combine-handler-clauses (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  orientation-stx
					  conversation-stx
					  #'pat-stx
					  reason-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((match-reason pat-stx inner-clause ...) outer-clause ...)
	 (append (combine-handler-clauses (syntax (inner-clause ...))
					  stateful?
					  state-stx
					  orientation-stx
					  conversation-stx
					  interest-type-stx
					  #'pat-stx)
		 (do-tail (syntax (outer-clause ...))))]

	[((on-presence expr ...) outer-clause ...)
	 (cons #`[(core:presence-event (core:role #,orientation-stx
						  #,conversation-stx
						  #,interest-type-stx))
		  #,(stateful-lift 'on-presence (syntax (expr ...)))]
	       (do-tail (syntax (outer-clause ...))))]

	[((on-absence expr ...) outer-clause ...)
	 (cons #`[(core:absence-event (core:role #,orientation-stx
						 #,conversation-stx
						 #,interest-type-stx)
				      #,reason-stx)
		  #,(stateful-lift 'on-absence (syntax (expr ...)))]
	       (do-tail (syntax (outer-clause ...))))]

	[((on-message [message-pat expr ...] ...) outer-clause ...)
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
		    [_ (lambda (state) (core:transition state '()))])]
	       (do-tail (syntax (outer-clause ...))))]

	[(unknown-clause outer-clause ...)
	 (raise-syntax-error #f
			     "Illegal clause in endpoint definition"
			     stx
			     #'unknown-clause)]))

    (syntax-case stx ()
      [(dummy pre-eid-exp role-exp handler-clause ...)
       #`(core:add-endpoint pre-eid-exp
			    role-exp
			    (match-lambda
			     #,@(reverse
				 (combine-handler-clauses
				  (syntax (handler-clause ...))
				  #f
				  (syntax old-state)
				  (syntax _)
				  (syntax _)
				  (syntax _)
				  (syntax _)))
			     [_ (lambda (state) (core:transition state '()))]))])))

;;; Local Variables:
;;; eval: (put 'name-endpoint 'scheme-indent-function 1)
;;; eval: (put 'let-fresh 'scheme-indent-function 1)
;;; eval: (put 'observe-subscribers 'scheme-indent-function 1)
;;; eval: (put 'observe-subscribers/everything 'scheme-indent-function 1)
;;; eval: (put 'observe-publishers 'scheme-indent-function 1)
;;; eval: (put 'observe-publishers/everything 'scheme-indent-function 1)
;;; eval: (put 'publish-on-topic 'scheme-indent-function 1)
;;; eval: (put 'subscribe-to-topic 'scheme-indent-function 1)
;;; eval: (put 'match-state 'scheme-indent-function 1)
;;; eval: (put 'match-orientation 'scheme-indent-function 1)
;;; eval: (put 'match-conversation 'scheme-indent-function 1)
;;; eval: (put 'match-interest-type 'scheme-indent-function 1)
;;; eval: (put 'match-reason 'scheme-indent-function 1)
;;; End:
