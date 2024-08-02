(post "bcsp-constraints.scm")

; constraint predicates filter things out, 
; returns true if valid or predicate should noop 
; return false if should filter out the assignment

(define (is-tonic? csp var val)
  (let* ((tonic (csp 'get-var 'tonic))
         (pitch (car val))
         (oct   (cdr val))
         (res   (eq? tonic pitch)))
    (post "(is-tonic?) var:" var "val:" val "tonic:" tonic "res:" res)
    res))

(define (above-oct-0? csp var val)
  (let* ((pitch (car val)) 
         (oct   (cdr val))
         (res   (> oct 0)))
    (post "(above-oct-0?) var:" var "val:" val "res:" res)
    res))

; dummy, incomplete. works on only two item list
(define (make-all-diff var-list)
  "return a predicate that runs over the var-list"
  (lambda (csp var val)
    #t))
        
