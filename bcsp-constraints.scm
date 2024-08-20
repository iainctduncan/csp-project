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

(define (is-chord-factor? csp var val factor)
  "return true if note pitch class is the factor of the chord"
  ;(post "(is-chord-factor?) var:" var "val:" val "factor:" factor)
  (let* ((root  (csp 'get-var 'root))
         (chord-q (csp 'get-var 'quality))
         (chord-pitches (root-chord->pitches root chord-q))
         (val-pitch (note->pitch val))
         (res (enh-eq? val-pitch (chord-pitches factor))))
    ;(post "  - var:" var "val:" val "factor:" factor "res:" res)
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
         (target-pitch (root-pitch key target-rnum))
         (res (enh-eq? target-pitch (note->pitch val))))
    ;(post "  target-pitch:" target-pitch "res:" res)
    res))       

;(define (target-neighbour? csp var val)
;  (post "(target-neighbour? var:" var "val:" val)
;  #t)

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

; diff is like all-diff but takes list of note vars
(define (diff? note-var-list)
  (lambda (csp notes)
    (post "diff? note-var-list" note-var-list "notes" notes) 
    (let ((len   (length notes))
          (pass  #t))
      (do ((i 0 (+ 1 i))) ((= i len))
        (do ((j 0 (+ 1 j))) ((= j len))
          (if (and 
                (in? i note-var-list) 
                (in? j note-var-list) 
                (eq? (notes i) (notes j)) 
                (not (= i j)) 
                (not (false? (notes i))))
            (begin 
              (post " - fail, i" i "j" j "in" (in? i note-var-list))
              (set! pass #f)))))
      pass)))

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


; for now this forces a chromatic neighbour only
(define (target-from-cn? csp notes)
  (if (or (not (csp 'get-var 'target)) (not (notes 3)) (not (notes 4)))
    #t
    (let* ((n-note (notes 3))
           (t-note (notes 4))
           (n-num (note->note-num n-note))
           (t-num (note->note-num t-note))
           (res (= 1 (abs (- t-num n-num)))))
      ;(post "(target-from-neighbour) n:" n-note "t:" t-note "res:" res)
      ;(post "  notes: " notes)
      res)))
