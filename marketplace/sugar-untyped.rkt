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
(require "sugar-values.rkt")

(provide (all-from-out "sugar-values.rkt")
	 (all-from-out "main.rkt")
	 ?
	 transition/no-state
	 endpoint
	 spawn
	 yield
	 nested-vm
	 ground-vm)

(define-syntax-rule (transition/no-state action ...)
  (transition (void) action ...))

;; A fresh unification variable, as identifier-syntax.
(define-syntax ? (syntax-id-rules () (_ (wild))))

(define-syntax endpoint
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~seq #:subscriber (~bind [is-subscriber #'#t]))
	       (~seq #:publisher (~bind [is-publisher #'#t])))
	  topic-expr
	  (~or (~seq #:participant (~bind [is-participant #'#t]))
	       (~seq #:observer (~bind [is-observer #'#t]))
	       (~seq #:everything (~bind [is-everything #'#t]))
	       (~seq))
	  (~or (~optional (~seq #:let-name name-binding)
			  #:defaults ([name-binding #'n0])
			  #:name "#:let-name binding for endpoint name")
	       (~optional (~seq #:name pre-eid) #:name "#:name of endpoint")
	       (~optional (~seq #:state state-pattern) #:name "#:state pattern")
	       (~optional (~seq #:on-presence presence) #:name "#:on-presence handler")
	       (~optional (~seq #:on-absence absence) #:name "#:on-absence handler")

	       (~optional (~seq #:role role) #:name "#:role")
	       (~optional (~seq #:peer-orientation orientation) #:name "#:peer-orientation")
	       (~optional (~seq #:conversation conversation) #:name "#:conversation")
	       (~optional (~seq #:peer-interest-type interest) #:name "#:peer-interest-type")

	       (~optional (~seq #:reason reason) #:defaults ([reason #'r0]) #:name "#:reason"))
	  ...
	  [message-pattern clause-body]
	  ...)
       (define-syntax-rule (build-handler event-pattern e-attr)
	 (if (attribute e-attr)
	     #`([event-pattern
		 #,(if (attribute state-pattern)
		       #`(match-lambda [state-pattern e-attr])
		       #`(lambda (state) (core:transition state e-attr)))])
	     #`([event-pattern (lambda (state) (core:transition state '()))])))
       (define role-pattern
	 (cond
	  [(attribute role)
	   (when (or (attribute orientation)
		     (attribute conversation)
		     (attribute interest))
	     (raise-syntax-error #f "Supply either #:role or any of (#:peer-orientation, #:conversation, #:peer-interest-type)" stx))
	   #'role]
	  [else
	   #`(core:role #,(if (attribute orientation) #'orientation #'_)
			#,(if (attribute conversation) #'conversation #'_)
			#,(if (attribute interest) #'interest #'_))]))
       #`(let ((name-binding #,(if (attribute pre-eid)
				   #'pre-eid
				   #'(gensym 'anonymous-endpoint))))
	   (core:add-endpoint
	    name-binding
	    (core:role #,(cond
			  [(attribute is-subscriber) #''subscriber]
			  [(attribute is-publisher) #''publisher]
			  [else (raise-syntax-error #f
						    "Missing #:subscriber or #:publisher"
						    stx)])
		       topic-expr
		       #,(cond
			  [(attribute is-participant) #''participant]
			  [(attribute is-observer) #''observer]
			  [(attribute is-everything) #''everything]
			  [else #''participant]))
	    (match-lambda
	     #,@(build-handler (core:presence-event #,role-pattern) presence)
	     #,@(build-handler (core:absence-event #,role-pattern reason) absence)
	     [(core:message-event #,role-pattern message)
	      #,(if (attribute state-pattern)
		    #`(match-lambda
		       [(and state state-pattern)
			(match message
			  [message-pattern clause-body] ...
			  [_ (core:transition state '())])])
		    #`(lambda (state)
			(core:transition state
					 (match message
					   [message-pattern clause-body] ...
					   [_ '()]))))]
	     [_
	      (lambda (state) (core:transition state '()))])))])))

(define-syntax spawn
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:pid pid) #:defaults ([pid #'p0]) #:name "#:pid")
	       (~optional (~seq #:debug-name debug-name)
			  #:defaults ([debug-name #'#f])
			  #:name "#:debug-name")) ...
	  (~or (~seq #:parent parent-state-pattern (~and (~not #:child) parent-k-exp))
	       (~seq #:parent (~and (~not #:child) parent-k-exp))
	       (~seq))
	  #:child exp)
       #`(core:spawn (core:process-spec (lambda (pid)
					  (lambda (k) (k exp))))
		     #,(if (attribute parent-k-exp)
			   (if (attribute parent-state-pattern)
			       #`(lambda (pid)
				   (match-lambda [parent-state-pattern parent-k-exp]))
			       #`(lambda (pid)
				   (lambda (state)
				     (core:transition state parent-k-exp))))
			   #'#f)
		     debug-name)])))

(define-syntax yield
  (lambda (stx)
    (syntax-case stx ()
      [(_ #:state state-pattern exp)
       #'(core:yield (match-lambda [state-pattern exp]))]
      [(_ exp)
       #'(core:yield (lambda (state) (core:transition state exp)))])))

(define-syntax nested-vm
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:let-pid vm-pid) #:defaults ([vm-pid #'p0])
			  #:name "#:vm-pid")
	       (~optional (~seq #:let-pid boot-pid) #:defaults ([boot-pid #'p0])
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
