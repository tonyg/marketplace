#lang marketplace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(observe-publishers (tcp-channel ? (tcp-listener 5999) ?)
		    (match-conversation (tcp-channel them us _)
					(on-presence (spawn (chat-session them us)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (chat-session them us)
  (define user (gensym 'user))
  (transition stateless
    (listen-to-user user them us)
    (speak-to-user user them us)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (listen-to-user user them us)
  (list

    (subscriber (tcp-channel them us ?)
      (on-absence (quit))
      (on-message
       [(tcp-channel _ _ (? bytes? text))
        (send-message `(,user says ,text))]))
   (publisher `(,user says ,?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (speak-to-user user them us)
  (define (say fmt . args)

     (send-message
      (tcp-channel us them (apply format fmt args))))
  (define (announce who did-what)
    (unless (equal? who user)
      (say "~s ~s.~n" who did-what)))
  (list
   (say "You are ~s.~n" user)

    (publisher (tcp-channel us them ?))
   (subscriber `(,? says ,?)
     (match-conversation `(,who says ,_)
       (on-presence (announce who 'arrived))
       (on-absence  (announce who 'departed))
       (on-message [`(,who says ,what)
                    (say "~a: ~a" who what)])))))
