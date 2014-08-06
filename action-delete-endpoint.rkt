#lang racket/base

(require racket/match)
(require "structs.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "process.rkt")
(require "quasiqueue.rkt")

(provide do-delete-endpoint
	 delete-all-endpoints)

;; do-delete-endpoint : (All (State) PreEID Reason (process State) vm
;; 			     -> (values (process State) vm))
(define (do-delete-endpoint pre-eid reason p state)
  (cond
   [(hash-has-key? (process-endpoints p) pre-eid)
    (define old-endpoint (hash-ref (process-endpoints p) pre-eid))
    (let-values (((p state) (notify-route-change-vm (remove-endpoint p old-endpoint)
						    old-endpoint
						    (lambda (t) (absence-event t reason))
						    state)))
      (values p state))]
   [else
    (values p state)]))

;; remove-endpoint : (All (State) (process State) (endpoint State) -> (process State))
(define (remove-endpoint p ep)
  (define pre-eid (eid-pre-eid (endpoint-id ep)))
  (struct-copy process p [endpoints (hash-remove (process-endpoints p) pre-eid)]))

;; delete-all-endpoints : (All (State) Reason (process State) vm
;; 			       -> (values (process State) vm (QuasiQueue (Action vm))))
(define (delete-all-endpoints reason p state)
  (let-values (((p state)
		(for/fold ([p p] [state state])
		    ([pre-eid (in-hash-keys (process-endpoints p))])
		  (do-delete-endpoint pre-eid reason p state))))
    (values p
	    state
	    (list->quasiqueue
	     (map (lambda (pre-eid)
		    (delete-endpoint (eid (process-pid p) pre-eid) reason))
		  (hash-keys (process-meta-endpoints p)))))))
