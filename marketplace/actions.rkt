#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "log-typed.rkt")
(require "process.rkt")
(require "action-add-endpoint.rkt")
(require "action-delete-endpoint.rkt")
(require "action-send-message.rkt")
(require "action-spawn.rkt")
(require "action-quit.rkt")
(require "list-utils.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(provide run-vm)

(: dump-state : vm -> Any)
(define (dump-state state)
  `(vm (next-pid ,(vm-next-process-id state))
       (processes ,@(for/fold: : Any
		        ([acc '()])
			([pid (in-hash-keys (vm-processes state))])
		      (cons (list pid (let ((wp (hash-ref (vm-processes state) pid)))
					(unwrap-process State Any (p wp)
					  (list (match (process-state p)
						  [(? vm? v) (dump-state v)]
						  [v v])
						(process-spawn-ks p)
						(process-endpoints p)
						(process-meta-endpoints p)
						(process-pending-actions p))))) acc)))))

(: run-vm : vm -> (Transition vm))
(define (run-vm state)
  ;; for each pid,
  ;; extract the corresponding process.
  ;; run through its work items, collecting external actions.
  ;; put the process back.
  ;; return the new state and the external actions
  (let next-process ((remaining-pids ((inst hash-keys PID Process) (vm-processes state)))
		     (state state)
		     (external-actions ((inst empty-quasiqueue (Action vm)))))
    (match remaining-pids
      ['()
       (let ((state (collect-dead-processes state))
	     (action-tree (quasiqueue->cons-tree external-actions)))
	 (transition state
		     (if (vm-idle? state)
			 action-tree
			 (cons (yield run-vm) action-tree))))]
      [(cons pid remaining-pids)
       (let-values (((state wp) (extract-process state pid)))
	 (if (not wp)
	     (next-process remaining-pids state external-actions)
	     (unwrap-process State (transition vm) (p wp)
	       (let next-action
		   ([remaining-actions (quasiqueue->list (process-pending-actions p))]
		    [p (reset-pending-actions p)]
		    [state state]
		    [external-actions external-actions])
		 (match remaining-actions
		   ['()
		    (next-process remaining-pids
				  (inject-process state (mkProcess p))
				  external-actions)]
		   [(cons action remaining-actions)
		    (matrix-log 'debug "PID ~v (~a) Action: ~v" pid (process-debug-name p) action)
		    (let-values (((p state new-external-actions)
				  (perform-action action p state)))
		      (if p
			  (next-action remaining-actions
				       p
				       state
				       (quasiqueue-append external-actions
							  new-external-actions))
			  (next-process remaining-pids
					state
					(quasiqueue-append external-actions
							   new-external-actions))))])))))])))

(: collect-dead-processes : vm -> vm)
(define (collect-dead-processes state)
  (: process-alive? : (All (State) (process State) -> Boolean))
  (define (process-alive? p)
    (or (not (null? (process-spawn-ks p)))
	(positive? (hash-count (process-endpoints p)))
	(positive? (hash-count (process-meta-endpoints p)))
	(not (quasiqueue-empty? (process-pending-actions p)))))
  (struct-copy vm state
    [processes (for/fold: : (HashTable PID Process)
		   ([processes (ann #hash() (HashTable PID Process))])
		   ([pid (in-hash-keys (vm-processes state))])
		 (define wp (hash-ref (vm-processes state) pid))
		 (unwrap-process State (HashTable PID Process) (p wp)
		   (if (process-alive? p)
		       (hash-set processes pid wp)
		       (begin (matrix-log 'info
					  "PID ~v (~a) garbage-collected"
					  pid
					  (process-debug-name p))
			      processes))))]))

(: vm-idle? : vm -> Boolean)
(define (vm-idle? state)
  (andmap (lambda (#{pid : PID})
	    (define wp (hash-ref (vm-processes state) pid))
	    (unwrap-process State Boolean (p wp)
	      (quasiqueue-empty? (process-pending-actions p))))
	  (hash-keys (vm-processes state))))

(: perform-action : (All (State) (Action State) (process State) vm
			 -> (Values (Option (process State)) vm (QuasiQueue (Action vm)))))
(define (perform-action action p state)
  (match action
    [(at-meta-level preaction)
     ((inst transform-meta-action State) preaction p state)]
    [(yield k)
     (let ((p (run-ready p k)))
       (values p state (empty-quasiqueue)))]
    [(quit maybe-pid reason)
     (do-quit (or maybe-pid (process-pid p)) reason p state)]
    [_
     (define-values (new-p new-state)
       (match action
	 [(add-endpoint pre-eid role handler)
	  (do-add-endpoint pre-eid role handler p state)]
	 [(delete-endpoint pre-eid reason)
	  (do-delete-endpoint pre-eid reason p state)]
	 [(send-message body orientation)
	  (do-send-message orientation body p state)]
	 [(spawn spec k debug-name)
	  (do-spawn spec k p debug-name state)]))
     (values new-p
	     new-state
	     (empty-quasiqueue))]))

(: wrap-trapk : eid -> (Handler vm))
(define (((wrap-trapk target-eid) event) state)
  (match-define (eid pid pre-eid) target-eid)
  (run-vm
   (let-values (((state wp) (extract-process state pid)))
     (if (not wp)
	 state
	 (unwrap-process State vm (p wp)
	   (define ep (hash-ref (process-meta-endpoints p) pre-eid always-false))
	   (if (not ep)
	       (inject-process state (mkProcess p))
	       (let ((p (run-ready p (send-to-user p (e) (quit-interruptk e)
				       ((endpoint-handler ep) event)))))
		 (inject-process state (mkProcess p)))))))))

(: dispatch-spawn-k : PID Integer -> (TrapK PID vm))
(define (((dispatch-spawn-k pid spawn-k-id) new-pid) state)
  (run-vm
   (let-values (((state wp) (extract-process state pid)))
     (if (not wp)
	 state
	 (unwrap-process State vm (p wp)
	   (match (assoc spawn-k-id (process-spawn-ks p))
	     [#f
	      (inject-process state (mkProcess p))]
	     [(and entry (cons _ k))
	      (define interruptk (send-to-user p (e) (quit-interruptk e)
				   (k new-pid)))
	      (define p1 (struct-copy process p [spawn-ks (remq entry (process-spawn-ks p))]))
	      (inject-process state (mkProcess (run-ready p1 interruptk)))]))))))

(: transform-meta-action : (All (State) (PreAction State) (process State) vm ->
				(Values (Option (process State)) vm (QuasiQueue (Action vm)))))
(define (transform-meta-action pa p state)
  (match pa
    [(add-endpoint pre-eid role unwrapped-handler)
     (define new-eid (eid (process-pid p) pre-eid))
     (values (struct-copy process p
			  [meta-endpoints (hash-set (process-meta-endpoints p)
						    pre-eid
						    ((inst endpoint State)
						     new-eid
						     role
						     unwrapped-handler))])
	     state
	     (quasiqueue
	      (add-endpoint (cast new-eid PreEID)
			    role
			    (wrap-trapk new-eid))))]
    [(delete-endpoint pre-eid reason)
     (define old-eid (eid (process-pid p) pre-eid))
     (values (struct-copy process p
			  [meta-endpoints (hash-remove (process-meta-endpoints p) pre-eid)])
	     state
	     (quasiqueue (delete-endpoint (cast old-eid PreEID) reason)))]
    [(send-message body orientation)
     (values p
	     state
	     (quasiqueue (send-message body orientation)))]
    [(spawn spec k debug-name)
     (define pid (process-pid p))
     (if k
	 (let ((spawn-k-id (+ 1 (list-max (map (inst car Integer (TrapK PID State))
					       (process-spawn-ks p))))))
	   (values (struct-copy process p
				[spawn-ks (cons (cons spawn-k-id k) (process-spawn-ks p))])
		   state
		   (quasiqueue (spawn spec (dispatch-spawn-k pid spawn-k-id) debug-name))))
	 (values p
		 state
		 (quasiqueue ((inst spawn vm) spec #f debug-name))))]
    [(quit maybe-pid reason)
     (values p
	     state
	     (quasiqueue (quit maybe-pid reason)))]))
