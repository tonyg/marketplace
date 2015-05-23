#lang racket/base

(require racket/match)
(require "structs.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "log.rkt")
(require "process.rkt")
(require "action-delete-endpoint.rkt")
(require "quasiqueue.rkt")

(provide do-quit)

;; do-quit : (All (State) PID Reason (process State) vm
;; 		  -> (values (Option (process State)) vm (QuasiQueue (Action vm))))
(define (do-quit killed-pid reason p state)

  ;; log-quit : (All (KilledState) (process KilledState) -> Void)
  (define (log-quit p)
    (marketplace-log (if reason 'warning 'info)
		     "PID ~v (~a) quits with reason: ~a"
		     killed-pid
		     (process-debug-name p)
		     (if (exn? reason)
                         (parameterize ([current-error-port (open-output-string)])
                           ((error-display-handler) (exn-message reason) reason)
                           (get-output-string (current-error-port)))
			 (format "~v" reason))))

  (if (equal? killed-pid (process-pid p))
      (let-values (((p state meta-actions) (delete-all-endpoints reason p state)))
	(log-quit p)
	(values #f state meta-actions))
      (let-values (((state maybe-killed-wp) (extract-process state killed-pid)))
	(if (not maybe-killed-wp)
	    (values p state (empty-quasiqueue))
	    (apply values
		   (let ((killed-p maybe-killed-wp))
		     (log-quit killed-p)
		     (let-values (((killed-p state meta-actions)
				   (delete-all-endpoints reason killed-p state)))
		       (list p state meta-actions))))))))
