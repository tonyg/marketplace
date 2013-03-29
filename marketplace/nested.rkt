#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "actions.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(provide make-nested-vm)

(: make-nested-vm : (All (State) (PID -> process-spec) Any -> (spawn State)))
(define (make-nested-vm make-boot debug-name)
  (spawn (process-spec (lambda (nested-vm-pid)
			 (lambda (k) ((inst k vm) (run-vm (make-vm (make-boot nested-vm-pid)))))))
	 #f
	 debug-name))

;; TODO: (process-spec: NewProcessState (pid-id) transition-expr)
