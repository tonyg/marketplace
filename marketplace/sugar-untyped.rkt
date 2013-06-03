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
(require "sugar-endpoints-untyped.rkt")

(provide (all-from-out "sugar-values.rkt")
	 (all-from-out "sugar-endpoints-untyped.rkt")
	 (all-from-out "main.rkt")
	 ?
	 transition/no-state
	 spawn
	 spawn/continue
	 name-process
	 yield
	 at-meta-level
	 nested-vm
	 ground-vm)

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

(define-syntax nested-vm
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
;;; eval: (put 'name-process 'scheme-indent-function 1)
;;; eval: (put 'yield 'scheme-indent-function 1)
;;; End:
