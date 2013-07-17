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
(require "sugar-endpoints-typed.rkt")

(provide (all-from-out "sugar-values.rkt")
	 (all-from-out "sugar-endpoints-typed.rkt")
	 (all-from-out "main.rkt")
	 ?
	 transition:
	 transition/no-state
	 spawn:
	 spawn/continue:
	 name-process
	 yield:
	 at-meta-level:
	 spawn-vm:
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

(define-syntax spawn:
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:pid pid) #:defaults ([pid #'p0]) #:name "#:pid")) ...
	  #:parent (~literal :) ParentState
	  #:child (~literal :) State exp)
       #`((inst core:spawn ParentState)
	  (core:process-spec (lambda (pid) (lambda (k) ((inst k State) exp))))
	  #f
	  #f)])))

(define-syntax spawn/continue:
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:pid pid) #:defaults ([pid #'p0]) #:name "#:pid")) ...
	  #:parent parent-state-pattern (~literal :) ParentState parent-k-exp
	  #:child (~literal :) State exp)
       #`((inst core:spawn ParentState)
	  (core:process-spec (lambda (pid) (lambda (k) ((inst k State) exp))))
	  (lambda (pid) (lambda: ([parent-state : ParentState])
			  (match parent-state [parent-state-pattern parent-k-exp])))
	  #f)])))

(: name-process : (All (State) Any (core:Spawn State) -> (core:Spawn State)))
(define (name-process n p)
  (match p
    [(core:spawn spec parent-k _)
     (core:spawn spec parent-k n)]))

(define-syntax yield:
  (lambda (stx)
    (syntax-case stx (:)
      [(_ state-pattern : State exp)
       #'((inst core:yield State) (lambda (state) (match state [state-pattern exp])))])))

(define-syntax at-meta-level:
  (lambda (stx)
    (syntax-case stx ()
      [(_ State)
       #''()]
      [(_ State preaction)
       #'((inst core:at-meta-level State) preaction)]
      [(_ State preaction ...)
       #'(list ((inst core:at-meta-level State) preaction) ...)])))

(define-syntax spawn-vm:
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
;;; eval: (put 'at-meta-level: 'scheme-indent-function 1)
;;; End:
