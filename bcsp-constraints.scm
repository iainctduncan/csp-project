(post "bcsp-constraints.scm")

; constraint predicates filter things out, 
; returns true if valid or predicate should noop 
; return false if should filter out the assignment

; TODO: prob want to make these some kind of object so we can get at their names...

(define (is-tonic? csp var val)
  "filter out first notes that are not the context tonic" 
  (post "first-note-tonic?")
  ; if not applied to first note, noop
  (if (not (= var 0))
    #t
    ; else passes if pitch-classes matches tonic
    (let* ((tonic (csp 'get-var 'tonic))
           (pitch (car val))
           (oct   (cdr val)))
      (eq? tonic pitch))))  

(define (is-tonic? csp var val)
  ; else passes if pitch-classes matches tonic
  (let* ((tonic (csp 'get-var 'tonic))
         (pitch (car val))
         (oct   (cdr val)))
    (eq? tonic pitch)))  

(define (above-first-octave? csp var val)
  (let* ((pitch (car val)) 
         (oct (cdr val)))
    (> oct 0)))  
