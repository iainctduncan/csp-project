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

; get pitch class for the third and see if it matches pitch class of note
; in prog
(define (is-chord-factor? csp var val factor)
  "return true if note pitch class is the third of the chord"
  ;(post "(is-chord-factor?) var:" var "val:" val "factor:" factor)
  (let* ((root  (csp 'get-var 'root))
         (chord-q (csp 'get-var 'quality))
         (chord-pitches (root-chord->pitches root chord-q))
         (val-pitch (note->pitch val))
         (res (enh-eq? val-pitch (chord-pitches factor))))
    ;(post "  (is-chord-factor? var:" var "val:" val "factor:" factor "res:" res)
    res))

(define (chord-root? csp var val)
  (is-chord-factor? csp var val 0))

(define (chord-3rd? csp var val)
  (is-chord-factor? csp var val 1))

(define (chord-5th? csp var val)
  (is-chord-factor? csp var val 2))

(define (chord-7th? csp var val)
  (is-chord-factor? csp var val 3))

(define (in-chord? csp var val)
  (or
    (is-chord-factor? csp var val 0)
    (is-chord-factor? csp var val 1)
    (is-chord-factor? csp var val 2)
    (is-chord-factor? csp var val 3)))

(define (in-triad? csp var val)
  (or
    (is-chord-factor? csp var val 0)
    (is-chord-factor? csp var val 1)
    (is-chord-factor? csp var val 2)))
    
(define (target-root? csp var val)
  "pass if pitch is the root of the target"
  ;(post "(target-root?)" val)
  (let* ((key (csp 'get-var 'tonic))
         (target-rnum (csp 'get-var 'target))
         (noop (post "  - " key target-rnum))
         (target-pitch (root-pitch key target-rnum))
         (res (enh-eq? target-pitch (note->pitch val))))
    ;(post "  target-pitch:" target-pitch "res:" res)
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


; checks intervals between all assigned notes
(define (intv-under? intv)
  (lambda (csp notes)
    ;(post "(intv-under) notes:" notes)
    (let* ((len (length notes))
           (max-semis (interval->semitones intv))
           (pass #t))
      (do ((i 0 (+ 1 i))) ((= i (- len 1)))
        (let* ((n1 (notes i))
               (n2 (notes (+ i 1)))
               (dist (if (and n1 n2) (semitones-between n1 n2) #f)))
          ;(post "  - " n1 n2 " dist:" dist "max:" max-semis)     
          (if (and dist (> dist max-semis))
            (set! pass #f))))        
      pass)))       
      
  
