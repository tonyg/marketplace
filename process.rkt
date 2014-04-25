#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "log-typed.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(provide send-to-user
	 send-to-user*
	 action-tree->quasiqueue
	 quit-interruptk
	 run-ready
	 notify-route-change-vm
	 marketplace-continuation-mark-key)

(define marketplace-continuation-mark-key (make-continuation-mark-key 'marketplace))

(define-syntax-rule (send-to-user p (e) failure-result enclosed-expr)
  (send-to-user* (process-debug-name p) (process-pid p) (e) failure-result enclosed-expr))

(define-syntax-rule (send-to-user* debug-name pid (e) failure-result enclosed-expr)
  (with-handlers ([exn:fail? (lambda: ([e : Reason])
			       (if (exn? e)
				   (marketplace-log 'error "Process ~v(~v):~n~a~n"
						    debug-name pid (exn-message e))
				   (marketplace-log 'error "Process ~v(~v):~n~v~n"
						    debug-name pid e))
			       failure-result)])
    (marketplace-log 'debug "Entering process ~v(~v)" debug-name pid)
    (define result (with-continuation-mark marketplace-continuation-mark-key
					   (or debug-name pid)
					   enclosed-expr))
    (marketplace-log 'debug "Leaving  process ~v(~v)" debug-name pid)
    result))

(: action-tree->quasiqueue : (All (State) (ActionTree State) -> (QuasiQueue (Action State))))
(define (action-tree->quasiqueue t)
  (let loop ((#{revacc : (QuasiQueue (Action State))} '()) (t t))
    ;; 1. Tried match with (define-predicate action? Action).
    ;;    Failed because of parametric function contracts.
    ;; 2. Tried flipping the order or clauses in the match to
    ;;    avoid the use of (action?), trying to pull out the
    ;;    false/nil/void leaving only, by exclusion, the
    ;;    Action. Failed, complaining that it didn't know the
    ;;    type in the third, default, branch.
    ;; 3. Just like 2, but with cond. This worked!
    (cond
     [(pair? t) (loop (loop revacc (car t)) (cdr t))]
     [(or (null? t) (eq? t #f) (void? t)) revacc]
     [else (cons t revacc)])))

;; Split out to provide a syntactic location to define State in
(: quit-interruptk : Reason -> (All (State) State -> (Transition State)))
(define ((quit-interruptk e) old-process-state)
  (transition old-process-state (quit #f e)))

(: run-ready : (All (State) (process State) (InterruptK State) -> (process State)))
(define (run-ready p interruptk)
  (define old-state (process-state p))
  (match-define (transition new-state actions)
    (send-to-user p (e) (transition old-state (ann (quit #f e) (Action State)))
      (interruptk old-state)))
  (struct-copy process p
	       [state new-state]
	       [pending-actions (quasiqueue-append (process-pending-actions p)
						   (action-tree->quasiqueue actions))]))

(: notify-route-change-self : (All (SNew)
				   (process SNew)
				   (endpoint SNew)
				   (Role -> EndpointEvent)
				   ->
				   (process SNew)))
(define (notify-route-change-self pn en flow->notification)
  (define endpointso (process-endpoints pn))
  (for/fold ([pn pn]) ([eido (in-hash-keys endpointso)])
    (define eo (hash-ref endpointso eido))
    (cond
     [(role-intersection (endpoint-role eo) (endpoint-role en))
      => (lambda (intersecting-topic)
	   (define flow-toward-o (refine-role (endpoint-role en) intersecting-topic))
	   (define flow-toward-n (refine-role (endpoint-role eo) intersecting-topic))
	   (invoke-handler-if-visible (invoke-handler-if-visible pn
								 eo
								 flow-toward-o
								 flow->notification)
				      en
				      flow-toward-n
				      flow->notification))]
     [else pn])))

(: notify-route-change-process : (All (SOld SNew)
				      (process SOld)
				      (process SNew)
				      (endpoint SNew)
				      (Role -> EndpointEvent)
				      -> (values (process SOld)
						 (process SNew))))
(define (notify-route-change-process po pn en flow->notification)
  (define endpointso (process-endpoints po))
  (for/fold ([po po]
	     [pn pn])
      ([eido (in-hash-keys endpointso)])
    (define eo (hash-ref endpointso eido))
    (cond
     [(role-intersection (endpoint-role eo) (endpoint-role en))
      => (lambda (intersecting-topic)
	   (define flow-toward-o (refine-role (endpoint-role en) intersecting-topic))
	   (define flow-toward-n (refine-role (endpoint-role eo) intersecting-topic))
	   (values (invoke-handler-if-visible po eo flow-toward-o flow->notification)
		   (invoke-handler-if-visible pn en flow-toward-n flow->notification)))]
     [else
      (values po pn)])))

(: invoke-handler-if-visible : (All (State)
				    (process State)
				    (endpoint State)
				    Role
				    (Role -> EndpointEvent)
				    ->
				    (process State)))
(define (invoke-handler-if-visible p ep flow flow->notification)
  (if (flow-visible? (endpoint-role ep) flow)
      (run-ready p (send-to-user p (e) (quit-interruptk e)
		     ((endpoint-handler ep) (flow->notification flow))))
      p))

(: notify-route-change-vm : (All (SNew)
				 (process SNew)
				 (endpoint SNew)
				 (Role -> EndpointEvent)
				 vm
				 -> (values (process SNew)
					    vm)))
(define (notify-route-change-vm pn en flow->notification state)
  (define old-processes (vm-processes state))
  (define-values (final-pn new-processes)
    (for/fold: : (values (process SNew)
			 (HashTable PID Process))
        ([pn (notify-route-change-self pn en flow->notification)]
	 [new-processes (ann #hash() (HashTable PID Process))])
	([pid (in-hash-keys old-processes)])
      (define wp (hash-ref old-processes pid))
      (apply values
	     (unwrap-process SOld (List (process SNew) (HashTable PID Process)) (po wp)
	       (let-values (((po pn) (notify-route-change-process po pn en flow->notification)))
		 (list pn (hash-set new-processes pid (mkProcess po))))))))
  (values final-pn
	  (struct-copy vm state [processes new-processes])))

;;; Local Variables:
;;; eval: (put 'send-to-user 'scheme-indent-function 3)
;;; End:
