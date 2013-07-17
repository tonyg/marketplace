#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "process.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(provide do-send-message)

(: do-send-message : (All (State) Orientation Message (process State) vm ->
			  (Values (Option (process State)) vm)))
(define (do-send-message orientation body sender-p state)
  (define message-role (role orientation body 'participant))

  (: send-to-process : (All (State) (process State) -> (process State)))
  (define (send-to-process p)
    (define endpoints (process-endpoints p))
    (for/fold: : (process State) ([p p])
	([eid (in-hash-keys endpoints)])
      (define e (hash-ref endpoints eid))
      (cond
       [(role-intersection message-role (endpoint-role e))
	(run-ready p (send-to-user p (e) (quit-interruptk e)
		       ((endpoint-handler e) (message-event message-role body))))]
       [else
	p])))

  ;; for each process in state (and also for p),
  (values (send-to-process sender-p)
	  (process-map send-to-process state)))
