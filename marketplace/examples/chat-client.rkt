#lang marketplace

(require racket/port)

;; Usually it's OK to just use display and friends directly.
;; Here we have a console output driver just to show how it's done.
(name-process 'console-output-driver
  (spawn (transition/no-state
	   (subscriber (list 'console-output ?)
	     (on-message [(list 'console-output item)
			  (printf "~a" item)
			  (void)])))))

(name-process 'console-input-driver
  (spawn (transition/no-state
	   (name-endpoint 'input-relay
	     (publisher (list 'console-input ?)
	       (on-absence
		(send-message (list 'console-output "Connection terminated.\n"))
		(quit))))
	   (subscriber (cons (read-line-evt (current-input-port) 'any) ?)
	     (on-message
	      [(cons _ (? eof-object?))
	       (send-message (list 'console-output "Terminating on local EOF.\n"))
	       (delete-endpoint 'input-relay)]
	      [(cons _ (? string? line))
	       (send-message (list 'console-input line))])))))

(name-process 'outbound-connection
  (spawn (let ((local (tcp-handle 'outbound))
	       (remote (tcp-address "localhost" 5999)))
	   (transition/no-state
	     (subscriber (list 'console-input ?)
	       (on-absence (quit))
	       (on-message
		[(list 'console-input line)
		 (send-message (list 'console-output (format "> ~a \n" line)))
		 (send-message (tcp-channel local remote (string-append line "\n")))]))
	     (publisher (tcp-channel local remote ?))
	     (subscriber (tcp-channel remote local ?)
	       (on-absence (quit))
	       (on-message
		[(tcp-channel _ _ (? eof-object?))
		 (quit)]
		[(tcp-channel _ _ data)
		 (send-message (list 'console-output (format "< ~a" data)))]))))))
