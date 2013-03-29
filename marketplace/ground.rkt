#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "log-typed.rkt")
(require "process.rkt")
(require "actions.rkt")
(require "action-send-message.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround
(require "support/event.rkt")

(require/typed typed/racket/base
	       [sync (Evt Evt * -> (vm -> vm))]
	       [never-evt Evt]
	       [always-evt Evt]
	       [wrap-evt (Evt (Any -> (vm -> vm)) -> Evt)])

(provide run-ground-vm)

(: run-ground-vm : process-spec -> Void)
(define (run-ground-vm boot)
  (let loop ((state (make-vm boot)))
    (match (run-vm state)
      [(transition state actions)
       (define is-blocking?
	 (match (quasiqueue->list (action-tree->quasiqueue actions))
	   ['()
	    ;; no "yield" action -> certainly blocking
	    #t]
	   [(list (yield (== run-vm)))
	    ;; single "yield", with k statically known to be run-vm -> poll
	    #f]
	   [_
	    ;; uh-oh
	    (error 'ground-vm
		   "Cannot process meta-actions ~v because no further metalevel exists"
		   actions)]))
       (define active-events
	 ((inst endpoint-fold (Listof Evt)) extract-ground-event-subscriptions '() state))
       (if (and is-blocking?
		(null? active-events))
	   (begin
	     ;; Not polling, and no events that could wake us from blocking, so quit
	     (matrix-log 'debug "Ground VM returning normally.")
	     (sleep 0.2) ;; give the log-receivers a chance to drain (!)
	     (void))
	   (let ((interruptk (apply sync
				    (if is-blocking?
					never-evt
					(wrap-evt always-evt (lambda (dummy) (inst values vm))))
				    active-events)))
	     (loop (interruptk state))))])))

(: extract-ground-event-subscriptions :
   (All (State) (process State) (endpoint State) (Listof Evt) -> (Listof Evt)))
(define (extract-ground-event-subscriptions old-p ep acc)
  (define pid (process-pid old-p))
  (match (endpoint-role ep)
    [(role 'subscriber (cons (? evt? evt) _) 'participant)
     (: evt-handler : Any -> vm -> vm)
     (define ((evt-handler message) state)
       (let-values (((state wp) (extract-process state pid)))
	 (if (not wp)
	     state
	     (unwrap-process State vm (p wp)
	       (let-values
		   (((p state)
		     (do-send-message 'publisher (cast (cons evt message) Message) p state)))
		 (if p
		     (inject-process state (mkProcess p))
		     state))))))
     (cons (wrap-evt evt evt-handler) acc)]
    [_ acc]))
