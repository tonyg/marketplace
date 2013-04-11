#lang typed/racket/base

(require racket/match)
(require "types.rkt")
(require "log-typed.rkt")
(require/typed "unify.rkt"
	       [wild (case-> (-> Topic) (Symbol -> Topic))]
	       [mgu-canonical (Topic Topic -> Topic)]
	       [freshen (Topic -> Topic)]
	       [specialization? (Topic Topic -> Boolean)])
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(provide co-orientations
	 co-roles
	 refine-role
	 roles-equal?
	 orientations-intersect?
	 role-intersection
	 flow-visible?)

(: co-orientations : Orientation -> (Listof Orientation))
(define (co-orientations o)
  (match o
    ['publisher '(subscriber)]
    ['subscriber '(publisher)]))

(: co-roles : Role -> (Listof Role))
(define (co-roles r)
  (for/list: ([co-orientation : Orientation (co-orientations (role-orientation r))])
    (struct-copy role r [orientation co-orientation])))

(: refine-role : Role Topic -> Role)
(define (refine-role remote-role new-topic)
  (struct-copy role remote-role [topic new-topic]))

(: roles-equal? : Role Role -> Boolean)
(define (roles-equal? ta tb)
  (and (equal? (role-orientation ta) (role-orientation tb))
       (equal? (role-interest-type ta) (role-interest-type tb))
       (specialization? (role-topic ta) (role-topic tb))
       (specialization? (role-topic tb) (role-topic ta))))

(: orientations-intersect? : Orientation Orientation -> Boolean)
(define (orientations-intersect? l r)
  (and (memq l (co-orientations r)) #t))

;; "Both left and right must be canonicalized." - comment from os2.rkt. What does it mean?
(: role-intersection : Role Role -> (Option Topic))
(define (role-intersection left right)
  (define result
    (and (orientations-intersect? (role-orientation left) (role-orientation right))
	 (mgu-canonical (freshen (role-topic left)) (freshen (role-topic right)))))
  (matrix-log 'debug "role-intersection ~v // ~v --> ~v" left right result)
  result)

;; True iff the flow between remote-role and local-role should be
;; visible to the local peer. This is the case when either local-role
;; is monitoring 'everything or otherwise if remote-role is a
;; 'participant only.
;;
;; |--------------+--------------+------------------------|
;; | local-role   | remote-role  | visible to local peer? |
;; |--------------+--------------+------------------------|
;; | 'participant | 'participant | yes                    |
;; | 'participant | 'observer    | no                     |
;; | 'participant | 'everything  | no                     |
;; | 'observer    | 'participant | yes                    |
;; | 'observer    | 'observer    | no                     |
;; | 'observer    | 'everything  | no                     |
;; | 'everything  | 'participant | yes                    |
;; | 'everything  | 'observer    | yes                    |
;; | 'everything  | 'everything  | yes                    |
;; |--------------+--------------+------------------------|
;;
(: flow-visible? : Role Role -> Boolean)
(define (flow-visible? local-role remote-role)
  (or (eq? (role-interest-type remote-role) 'participant)
      (eq? (role-interest-type local-role) 'everything)))
