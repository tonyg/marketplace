#lang racket/base

(require (for-syntax syntax/parse))
(require (for-syntax racket/base))

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
(require "support/dsl-untyped.rkt")

(provide (all-from-out "main.rkt")

	 transition
	 delete-endpoint
	 send-message
	 send-feedback
	 quit
	 sequence-actions
	 (rename-out [core:wild wild])

	 name-endpoint
	 let-fresh
	 observe-subscribers
	 observe-subscribers/everything
	 observe-publishers
	 observe-publishers/everything
	 publisher
	 subscriber
	 build-endpoint

	 ?
	 transition/no-state
	 spawn
	 spawn/continue
	 name-process
	 yield
	 at-meta-level
	 spawn-vm
	 ground-vm)

;; transition : (All (State) State (core:ActionTree State) * -> (core:Transition State))
(define (transition state . actions)
  (core:transition state actions))

(define (delete-endpoint id [reason #f])
  (core:delete-endpoint id reason))

;; send-message : (case-> [Any -> core:send-message]
;; 			  [Any core:Orientation -> core:send-message])
(define (send-message body [orientation 'publisher])
  (core:send-message body orientation))

(define (send-feedback body)
  (core:send-message body 'subscriber))

;; quit : (case-> [-> core:quit]
;; 		  [(Option core:PID) -> core:quit]
;; 		  [(Option core:PID) Any -> core:quit])
(define (quit [who #f] [reason #f])
  (core:quit who reason))

;; sequence-actions : (All (State)
;; 			   (core:Transition State)
;; 			   (U (core:ActionTree State) (State -> (core:Transition State))) *
;; 			   -> (core:Transition State))
(define (sequence-actions t . more-actions-and-transformers)
  (match-define (core:transition initial-state initial-actions) t)
  (let loop ((state initial-state)
	     (actions initial-actions)
	     (items more-actions-and-transformers))
    (match items
      ['()
       (core:transition state actions)]
      [(cons item remaining-items)
       (if (procedure? item)
	   (match (item state)
	     [(core:transition new-state more-actions)
	      (loop new-state
		    (cons actions more-actions)
		    remaining-items)])
	   (loop state
		 (cons actions item)
		 remaining-items))])))

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

(define-syntax-rule (publisher topic clause ...)
  (build-endpoint (gensym 'anonymous-endpoint)
		  (core:role 'publisher topic 'participant)
		  clause ...))

(define-syntax-rule (subscriber topic clause ...)
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

(define-syntax-rule (transition/no-state action ...)
  (transition (void) action ...))

;; A fresh unification variable, as identifier-syntax.
(define-syntax ? (syntax-id-rules () (_ (wild))))

(define-syntax spawn
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:pid pid) #:defaults ([pid #'p0]) #:name "#:pid")) ...
	  exp)
       #`(core:spawn (core:process-spec (lambda (pid) (lambda (k) (k exp))))
		     #f
		     #f)])))

(define-syntax spawn/continue
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:pid pid) #:defaults ([pid #'p0]) #:name "#:pid")) ...
	  #:parent parent-state-pattern parent-k-exp
	  #:child exp)
       #`(core:spawn (core:process-spec (lambda (pid) (lambda (k) (k exp))))
		     (lambda (pid) (match-lambda [parent-state-pattern parent-k-exp]))
		     #f)])))

(define (name-process n p)
  (match p
    [(core:spawn spec parent-k _)
     (core:spawn spec parent-k n)]))

(define-syntax yield
  (lambda (stx)
    (syntax-case stx ()
      [(_ state-pattern exp)
       #'(core:yield (match-lambda [state-pattern exp]))])))

(define (at-meta-level . preactions)
  (match preactions
    [(cons preaction '()) (core:at-meta-level preaction)]
    [_ (map core:at-meta-level preactions)]))

(define-syntax spawn-vm
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:vm-pid vm-pid) #:defaults ([vm-pid #'p0])
			  #:name "#:vm-pid")
	       (~optional (~seq #:boot-pid boot-pid) #:defaults ([boot-pid #'p0])
			  #:name "#:boot-pid")
	       (~optional (~seq #:initial-state initial-state)
			  #:defaults ([initial-state #'(void)])
			  #:name "#:initial-state")
	       (~optional (~seq #:debug-name debug-name)
			  #:defaults ([debug-name #'#f])
			  #:name "#:debug-name"))
	  ...
	  exp ...)
       #`(core:make-nested-vm
	  (lambda (vm-pid)
	    (core:process-spec (lambda (boot-pid)
				 (lambda (k) (k (core:transition initial-state
								 (list exp ...)))))))
	  debug-name)])))

(define-syntax ground-vm
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:boot-pid boot-pid) #:defaults ([boot-pid #'p0])
			  #:name "#:boot-pid")
	       (~optional (~seq #:initial-state initial-state)
			  #:defaults ([initial-state #'(void)])
			  #:name "#:initial-state"))
	  ...
	  exp ...)
       #`(core:run-ground-vm
	  (core:process-spec (lambda (boot-pid)
			       (lambda (k) (k (core:transition initial-state
							       (list exp ...)))))))])))

;;; Local Variables:
;;; eval: (put 'sequence-actions 'scheme-indent-function 1)
;;; eval: (put 'name-process 'scheme-indent-function 1)
;;; eval: (put 'yield 'scheme-indent-function 1)
;;; eval: (put 'name-endpoint 'scheme-indent-function 1)
;;; eval: (put 'let-fresh 'scheme-indent-function 1)
;;; eval: (put 'observe-subscribers 'scheme-indent-function 1)
;;; eval: (put 'observe-subscribers/everything 'scheme-indent-function 1)
;;; eval: (put 'observe-publishers 'scheme-indent-function 1)
;;; eval: (put 'observe-publishers/everything 'scheme-indent-function 1)
;;; eval: (put 'publisher 'scheme-indent-function 1)
;;; eval: (put 'subscriber 'scheme-indent-function 1)
;;; eval: (put 'match-state 'scheme-indent-function 1)
;;; eval: (put 'match-orientation 'scheme-indent-function 1)
;;; eval: (put 'match-conversation 'scheme-indent-function 1)
;;; eval: (put 'match-interest-type 'scheme-indent-function 1)
;;; eval: (put 'match-reason 'scheme-indent-function 1)
;;; End:
