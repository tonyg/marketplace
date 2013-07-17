#lang racket/base
;; Untyped-Racket support for Timer driver.

(require "timer.rkt")
(provide (except-out (all-from-out "timer.rkt")
		     set-timer
		     set-timer-pattern
		     timer-expired
		     timer-expired-pattern)
	 (rename-out [set-timer-repr set-timer]
		     [set-timer-repr set-timer-pattern]
		     [timer-expired-repr timer-expired]
		     [timer-expired-repr timer-expired-pattern]))
