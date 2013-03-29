#lang typed/racket/base

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
		    quit
		    wild))
(require "sugar-values.rkt")

(provide (all-from-out "sugar-values.rkt")
	 (all-from-out "main.rkt")
	 ?
	 transition:
	 transition/no-state
	 endpoint:
	 spawn:
	 yield:
	 at-meta-level:
	 nested-vm:
	 ground-vm:)

;; A fresh unification variable, as identifier-syntax.
(define-syntax ? (syntax-id-rules () (_ (wild))))

(define-syntax transition:
  (lambda (stx)
    (syntax-case stx (:)
      [(_ state : State action ...)
       #'((inst transition State) state action ...)])))

(define-syntax-rule (transition/no-state action ...)
  (transition: (void) : Void action ...))

(define-syntax endpoint:
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~seq (~literal :) State)
	       (~seq state-pattern (~literal :) State))
	  (~or (~seq #:subscriber (~bind [is-subscriber #'#t]))
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
		       #`(lambda: ([state : State]) (match state [state-pattern e-attr]))
		       #`(lambda: ([state : State]) ((inst core:transition State) state e-attr)))])
	     #`([event-pattern (lambda: ([state : State])
				 ((inst core:transition State) state '()))])))
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
       #`(let ((name-binding (cast #,(if (attribute pre-eid)
					 #'pre-eid
					 #'(gensym 'anonymous-endpoint))
				   core:PreEID)))
	   (core:add-endpoint
	    name-binding
	    (core:role #,(cond
			  [(attribute is-subscriber) #''subscriber]
			  [(attribute is-publisher) #''publisher]
			  [else (raise-syntax-error #f
						    "Missing #:subscriber or #:publisher"
						    stx)])
		       (cast topic-expr core:Topic)
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
		    #`(lambda: ([state : State])
			(match state
			  [state-pattern
			   (match message
			     [message-pattern clause-body] ...
			     [_ ((inst core:transition State) state '())])]))
		    #`(lambda: ([state : State])
			((inst core:transition State)
			 state
			 (match message
			   [message-pattern clause-body] ...
			   [_ '()]))))])))])))

(define-syntax spawn:
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:pid pid) #:defaults ([pid #'p0]) #:name "#:pid")
	       (~optional (~seq #:debug-name debug-name)
			  #:defaults ([debug-name #'#f])
			  #:name "#:debug-name")) ...
	  (~or (~seq #:parent parent-state-pattern (~literal :) ParentState
		     (~and (~not #:child) parent-k-exp))
	       (~seq #:parent (~literal :) ParentState
		     (~and (~not #:child) parent-k-exp))
	       (~seq #:parent (~literal :) ParentState))
	  #:child (~literal :) State exp)
       #`((inst core:spawn ParentState)
	  (core:process-spec (lambda (pid)
			       (lambda (k) ((inst k State) exp))))
	  #,(if (attribute parent-k-exp)
		(if (attribute parent-state-pattern)
		    #`(lambda (pid)
			(lambda: ([parent-state : ParentState])
			  (match parent-state [parent-state-pattern parent-k-exp])))
		    #`(lambda (pid)
			(lambda: ([parent-state : ParentState])
			  ((inst core:transition ParentState) parent-state parent-k-exp))))
		#'#f)
	  debug-name)])))

(define-syntax yield:
  (lambda (stx)
    (syntax-case stx (:)
      [(_ state-pattern : State exp)
       #'((inst core:yield State) (lambda (state) (match state [state-pattern exp])))]
      [(_ : State exp)
       #'((inst core:yield State) (lambda (state) (core:transition state exp)))])))

(define-syntax at-meta-level:
  (lambda (stx)
    (syntax-case stx (:)
      [(_ : State preaction ...)
       #'((inst at-meta-level State) preaction ...)])))

(define-syntax nested-vm:
  (lambda (stx)
    (syntax-parse stx
      [(_ (~literal :) ParentState
	  (~or (~optional (~seq #:vm-pid vm-pid) #:defaults ([vm-pid #'p0])
			  #:name "#:vm-pid")
	       (~optional (~seq #:boot-pid boot-pid) #:defaults ([boot-pid #'p0])
			  #:name "#:boot-pid")
	       (~optional (~seq #:initial-state initial-state (~literal :) InitialState)
			  #:defaults ([initial-state #'(void)] [InitialState #'Void])
			  #:name "#:initial-state")
	       (~optional (~seq #:debug-name debug-name)
			  #:defaults ([debug-name #'#f])
			  #:name "#:debug-name"))
	  ...
	  exp ...)
       #`((inst core:make-nested-vm ParentState)
	  (lambda (vm-pid)
	    (core:process-spec (lambda (boot-pid)
				 (lambda (k) ((inst k InitialState)
					      (core:transition initial-state
							       (list exp ...)))))))
	  debug-name)])))

(define-syntax ground-vm:
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:boot-pid boot-pid) #:defaults ([boot-pid #'p0])
			  #:name "#:boot-pid")
	       (~optional (~seq #:initial-state initial-state (~literal :) InitialState)
			  #:defaults ([initial-state #'(void)] [InitialState #'Void])
			  #:name "#:initial-state"))
	  ...
	  exp ...)
       #`(core:run-ground-vm
	  (core:process-spec (lambda (boot-pid)
			       (lambda (k) ((inst k InitialState)
					    (core:transition initial-state
							     (list exp ...)))))))])))

;;; Local Variables:
;;; eval: (put 'at-meta-level: 'scheme-indent-function 2)
;;; End:
