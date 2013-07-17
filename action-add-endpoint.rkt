#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "process.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(provide do-add-endpoint)

(: do-add-endpoint : (All (State) PreEID Role (Handler State) (process State) vm
			  -> (values (Option (process State)) vm)))
(define (do-add-endpoint pre-eid role h p state)
  (define new-eid (eid (process-pid p) pre-eid))
  (define old-endpoint (hash-ref (process-endpoints p) pre-eid (lambda () #f)))
  (define new-endpoint (endpoint new-eid role h))
  (if old-endpoint
      ;; We are *updating* an existing endpoint's behaviour.
      (if (roles-equal? (endpoint-role old-endpoint)
			(endpoint-role new-endpoint))
	  (values (install-endpoint p new-endpoint)
		  state)
	  ;; TODO: Make this error fatal for the process, not the VM!
	  (error 'do-add-endpoint
		 "Roles must be equal when updating an endpoint: ~v vs ~v"
		 old-endpoint
		 new-endpoint))
      ;; We are installing a *new* endpoint.
      ;; TODO: Decide whether to signal a process' endpoints about
      ;; *its own* matching endpoints.
      (let-values (((p state) (notify-route-change-vm (install-endpoint p new-endpoint)
						      new-endpoint
						      presence-event
						      state)))
	(values p state))))

(: install-endpoint : (All (State) (process State) (endpoint State) -> (process State)))
(define (install-endpoint p ep)
  (define pre-eid (eid-pre-eid (endpoint-id ep)))
  (struct-copy process p [endpoints (hash-set (process-endpoints p) pre-eid ep)]))
