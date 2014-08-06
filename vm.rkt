#lang racket/base

(require racket/match)
(require "structs.rkt")
(require "roles.rkt")
(require "quasiqueue.rkt")

(provide vm-processes ;; (struct-out vm) doesn't work because of make-vm below (See PR13161)
	 vm-next-process-id
	 vm
	 vm?

	 (struct-out process)
	 (struct-out endpoint)
	 (struct-out eid)

	 make-vm
	 inject-process
	 extract-process
	 always-false
	 reset-pending-actions
	 process-map
	 endpoint-fold)

(struct vm (processes ;; (HashTable PID Process)
	    next-process-id ;; PID
	    )
	#:transparent)

(struct process (debug-name ;; Any
		 pid ;; PID
		 state ;; State
		 spawn-ks ;; (Listof (Pairof Integer (TrapK PID State))) ;; hmm
		 endpoints ;; (HashTable PreEID (endpoint State))
		 meta-endpoints ;; (HashTable PreEID (endpoint State))
		 pending-actions ;; (QuasiQueue (Action State))
		 )
	#:transparent)

(struct endpoint (id ;; eid
		  role ;; role
		  handler ;; (Handler State)
		  )
	#:transparent)

(struct eid (pid ;; PID
	     pre-eid ;; PreEID
	     )
	#:transparent)

;;---------------------------------------------------------------------------

;; make-vm : process-spec -> vm
(define (make-vm boot)
  (define primordial (process '#:primordial
			      -1
			      (void)
			      (list)
			      #hash()
			      #hash()
			      (quasiqueue (spawn boot #f '#:boot-process))))
  (vm (hash-set #hash() (process-pid primordial) primordial) 0))

;; inject-process : vm Process -> vm
(define (inject-process state wp)
  (struct-copy vm state [processes (hash-set (vm-processes state) (process-pid wp) wp)]))

;; always-false : -> False
(define (always-false) #f)

;; extract-process : vm PID -> (values vm (Option Process))
(define (extract-process state pid)
  (define wp (hash-ref (vm-processes state) pid always-false))
  (values (if wp
	      (struct-copy vm state [processes (hash-remove (vm-processes state) pid)])
	      state)
	  wp))

;; reset-pending-actions : (All (State) (process State) -> (process State))
(define (reset-pending-actions p)
  (struct-copy process p [pending-actions (empty-quasiqueue)]))

;; process-map : (All (State) (process State) -> (process State)) vm -> vm
;; TODO: simplify
(define (process-map f state)
  (for/fold ([state state]) ([pid (in-hash-keys (vm-processes state))])
    (let-values (((state wp) (extract-process state pid)))
      (if (not wp)
	  state
	  (inject-process state (f wp))))))

;; endpoint-fold : (All (A) (All (State) (process State) (endpoint State) A -> A) A vm -> A)
(define (endpoint-fold f seed state)
  (for/fold ([seed seed]) ([pid (in-hash-keys (vm-processes state))])
    (let-values (((state wp) (extract-process state pid)))
      (if (not wp)
	  seed
	  (for/fold ([seed seed]) ([pre-eid (in-hash-keys (process-endpoints wp))])
	    (define ep (hash-ref (process-endpoints wp) pre-eid))
	    (f wp ep seed))))))

;;; Local Variables:
;;; eval: (put 'unwrap-process 'scheme-indent-function 3)
;;; End:
