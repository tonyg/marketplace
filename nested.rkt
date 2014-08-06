#lang racket/base

(require racket/match)
(require "structs.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "actions.rkt")

(provide make-nested-vm)

;; make-nested-vm : (All (State) (PID -> process-spec) Any -> (spawn State))
(define (make-nested-vm make-boot debug-name)
  (spawn (process-spec (lambda (nested-vm-pid)
			 (lambda (k) (k (run-vm (make-vm (make-boot nested-vm-pid)))))))
	 #f
	 debug-name))

;; TODO: (process-spec: NewProcessState (pid-id) transition-expr)
