#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "log-typed.rkt")
(require "process.rkt")
(require "action-delete-endpoint.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(require/typed web-server/private/util
	       [exn->string (exn -> String)])

(provide do-quit)

(: do-quit : (All (State) PID Reason (process State) vm
		  -> (values (Option (process State)) vm (QuasiQueue (Action vm)))))
(define (do-quit killed-pid reason p state)

  (: log-quit : (All (KilledState) (process KilledState) -> Void))
  (define (log-quit p)
    (marketplace-log (if reason 'warning 'info)
		     "PID ~v (~a) quits with reason: ~a"
		     killed-pid
		     (process-debug-name p)
		     (if (exn? reason)
			 (exn->string reason)
			 (format "~v" reason))))

  (if (equal? killed-pid (process-pid p))
      (let-values (((p state meta-actions) (delete-all-endpoints reason p state)))
	(log-quit p)
	(values #f state meta-actions))
      (let-values (((state maybe-killed-wp) (extract-process state killed-pid)))
	(if (not maybe-killed-wp)
	    (values p state (empty-quasiqueue))
	    (apply values
		   (unwrap-process KilledState
		       (List (Option (process State)) vm (QuasiQueue (Action vm)))
		       (killed-p maybe-killed-wp)
		     (log-quit killed-p)
		     (let-values (((killed-p state meta-actions)
				   (delete-all-endpoints reason killed-p state)))
		       (list p state meta-actions))))))))
