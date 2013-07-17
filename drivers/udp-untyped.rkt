#lang racket/base
;; UDP driver. Untyped macro wrappers

(require "udp.rkt")
(provide (except-out (all-from-out "udp.rkt")
		     udp-remote-address
		     udp-remote-address-pattern
		     udp-handle
		     udp-handle-pattern
		     udp-listener
		     udp-listener-pattern
		     udp-packet
		     udp-packet-pattern)
	 (rename-out [udp-remote-address-repr udp-remote-address]
		     [udp-remote-address-repr udp-remote-address-pattern]
		     [udp-handle-repr udp-handle]
		     [udp-handle-repr udp-handle-pattern]
		     [udp-listener-repr udp-listener]
		     [udp-listener-repr udp-listener-pattern]
		     [udp-packet-repr udp-packet]
		     [udp-packet-repr udp-packet-pattern]))
