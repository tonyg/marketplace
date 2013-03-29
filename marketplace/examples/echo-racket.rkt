#lang racket

(define (connection-handler cin cout)
  (let loop ()
    (define ch (read-char cin))
    (when (not (eof-object? ch))
      (display ch cout)
      (flush-output cout)
      (loop))))

(let ((listener (tcp-listen 5999 4 #t)))
  (let loop ()
    (define-values (cin cout) (tcp-accept listener))
    (thread (lambda () (connection-handler cin cout)))
    (loop)))
