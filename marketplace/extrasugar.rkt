#lang racket/base

(require (for-syntax syntax/parse))
(require (for-syntax racket/base))

(require racket/stxparam)
(require racket/splicing)

(require racket/match)

(require (prefix-in core: "main.rkt"))
(require (except-in "main.rkt"
		    at-meta-level
		    spawn
		    yield
		    transition
		    delete-endpoint
		    send-message
		    quit))
(require "sugar-untyped.rkt")

(provide (except-out (all-from-out "sugar-untyped.rkt") endpoint)
	 (all-from-out "main.rkt")
	 name-endpoint
	 let-fresh
	 observe-subscribers
	 observe-publishers
	 publish-on-topic
	 subscribe-to-topic
	 build-endpoint)

(define-syntax-rule (define&provide-endpoint-helper-syntaxes identifier ...)
  (begin (provide identifier ...)
	 (define-syntax identifier
	   (lambda (stx)
	     (raise-syntax-error #f
				 (format "Illegal use of ~a outside endpoint definition context"
					 'identifier)
				 stx)))
	 ...))

(define&provide-endpoint-helper-syntaxes
  match-state
  match-orientation
  match-conversation
  match-interest-type
  match-reason
  on-presence
  on-absence
  on-message)

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

(define-syntax-rule (observe-publishers topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'subscriber topic 'observer)
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

      (define (stateful-lift expr-stx)
	(if stateful?
	    #`(match-lambda [#,state-stx #,expr-stx])
	    #`(lambda (state) (core:transition state #,expr-stx))))

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
		  #,(stateful-lift (syntax (begin expr ...)))]
	       (do-tail (syntax (outer-clause ...))))]

	[((on-absence expr ...) outer-clause ...)
	 (cons #`[(core:absence-event (core:role #,orientation-stx
						 #,conversation-stx
						 #,interest-type-stx)
				      #,reason-stx)
		  #,(stateful-lift (syntax (begin expr ...)))]
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
				 #`[message-pat #,(stateful-lift (syntax (begin expr ...)))])))
			    (syntax->list (syntax ([message-pat expr ...] ...))))
		    [_ (lambda (state) (core:transition state '()))])]
	       (do-tail (syntax (outer-clause ...))))]))

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
