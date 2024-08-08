(post "bcsp-constraints.scm")

; constraint predicates filter things out, 
; returns true if valid or predicate should noop 
; return false if should filter out the assignment

(define (is-tonic? csp var val)
  ;(post "(is-tonic?) var:" var "val:" val)
  (let* ((tonic (csp 'get-var 'tonic))
         (res   (eq? tonic (note->pitch val))))
    ;(post "(is-tonic?) var:" var "val:" val "tonic:" tonic "res:" res)
    res))

(define (above-oct-0? csp var val)
  ;(post "(above-oct-0?) var:" var "val:" val)
  (let* ((res  (> (note->oct val) 0)))
    ;(post "(above-oct-0?) var:" var "val:" val "res:" res)
    res))

;********************************************************************************
; global constraints
; these must be prepared to run over missing note slots

; fail on any already assigned notes that are duplicates
(define (all-diff? csp notes)
  (let ((len   (length notes))
        (pass  #t))
    (do ((i 0 (+ 1 i))) ((= i len))
      (do ((j 0 (+ 1 j))) ((= j len))
        (if (and (eq? (notes i) (notes j)) (not (= i j)) (not (false? (notes i))))
          (set! pass #f))))
    pass))
