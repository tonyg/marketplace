#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(provide vm-processes ;; (struct-out vm) doesn't work because of make-vm below (See PR13161)
	 vm-next-process-id
	 vm ;; really just want to export the type here, not the ctor
	 vm?

	 (struct-out process)
	 (struct-out endpoint)
	 (struct-out eid)
	 Process
	 CoProcess
	 mkProcess
	 unwrap-process

	 make-vm
	 inject-process
	 extract-process
	 always-false
	 reset-pending-actions
	 process-map
	 endpoint-fold)

(struct: vm ([processes : (HashTable PID Process)]
	     [next-process-id : PID])
	 #:transparent)

(struct: (State)
	 process ([debug-name : Any]
		  [pid : PID]
		  [state : State]
		  [spawn-ks : (Listof (Pairof Integer (TrapK PID State)))] ;; hmm
		  [endpoints : (HashTable PreEID (endpoint State))]
		  [meta-endpoints : (HashTable PreEID (endpoint State))]
		  [pending-actions : (QuasiQueue (Action State))])
	 #:transparent)

(struct: (State)
	 endpoint ([id : eid]
		   [role : role]
		   [handler : (Handler State)])
	 #:transparent)

(struct: eid ([pid : PID]
	      [pre-eid : PreEID])
	 #:prefab)

(define-type Process		(All (R) (CoProcess R) -> R))
(define-type (CoProcess R)	(All (State) (process State) -> R))

(: mkProcess : (All (State) ((CoProcess Process) State)))
;; A kind of identity function, taking the components of a process to
;; a process.
(define (mkProcess p)
  (lambda (k) ((inst k State) p)))

(: Process-pid : Process -> PID)
(define (Process-pid wp) ((inst wp PID) process-pid))

;; Unwraps a process. Result is the type of the result of the
;; expression; State is a type variable to be bound to the process's
;; private state type. p is to be bound to the unwrapped process; wp
;; is the expression producing the wrapped process. body... are the
;; forms computing a value of type Result.
(define-syntax-rule (unwrap-process State Result (p wp) body ...)
  (let ()
    (: coproc : (All (State) (process State) -> Result))
    (define (coproc p)
      body ...)
    ((inst wp Result) coproc)))

;;---------------------------------------------------------------------------

(: make-vm : process-spec -> vm)
(define (make-vm boot)
  (define primordial (mkProcess ((inst process Void)
				 '#:primordial
				 -1
				 (void)
				 (list)
				 #hash()
				 #hash()
				 (quasiqueue ((inst spawn Void) boot #f '#:boot-process)))))
  (vm (hash-set (ann #hash() (HashTable PID Process))
		(Process-pid primordial)
		primordial)
      0))

(: inject-process : vm Process -> vm)
(define (inject-process state wp)
  (struct-copy vm state [processes (hash-set (vm-processes state) (Process-pid wp) wp)]))

(: always-false : -> False)
(define (always-false) #f)

(: extract-process : vm PID -> (values vm (Option Process)))
(define (extract-process state pid)
  (define wp (hash-ref (vm-processes state) pid always-false))
  (values (if wp
	      (struct-copy vm state [processes (hash-remove (vm-processes state) pid)])
	      state)
	  wp))

(: reset-pending-actions : (All (State) (process State) -> (process State)))
(define (reset-pending-actions p)
  (struct-copy process p [pending-actions ((inst empty-quasiqueue (Action State)))]))

(: process-map : (All (State) (process State) -> (process State)) vm -> vm)
(define (process-map f state)
  (for/fold ([state state]) ([pid (in-hash-keys (vm-processes state))])
    (let-values (((state wp) (extract-process state pid)))
      (if (not wp)
	  state
	  (unwrap-process State vm (p wp)
	    (inject-process state (mkProcess (f p))))))))

(: endpoint-fold : (All (A) (All (State) (process State) (endpoint State) A -> A) A vm -> A))
(define (endpoint-fold f seed state)
  (for/fold ([seed seed]) ([pid (in-hash-keys (vm-processes state))])
    (let-values (((state wp) (extract-process state pid)))
      (if (not wp)
	  seed
	  (unwrap-process State A (p wp)
	    (for/fold ([seed seed]) ([pre-eid (in-hash-keys (process-endpoints p))])
	      (define ep (hash-ref (process-endpoints p) pre-eid))
	      ((inst f State) p ep seed)))))))

;;; Local Variables:
;;; eval: (put 'unwrap-process 'scheme-indent-function 3)
;;; End:
