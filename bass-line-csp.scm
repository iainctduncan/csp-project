; csp-1 a brute force backtracking search to arpeggiate chords
(post "csp-1 loading")
(load-from-max "stuff.scm")
(load-from-max "bcsp-helpers.scm")
(load-from-max "bcsp-constraints.scm")
(define (s4m-reset) (delay 100 (lambda()(send 'reset 'bang))))


;*********************************************************************************
; my message passing csp object
; note that this implementation means #f is not a valid domain value!
(define (make-csp) 
  (let ((_ (hash-table 
    ; variable names, numbers correspond to notes
    :vars         '(tonic tonality root quality 0 1 2 3)   
    :context-vars '(tonic tonality root quality)
    :note-vars    '(0 1 2 3)
    ; assigned vals is a hash-table keyed by var name (or number)
    :assigned     (hash-table)  
    ; constraints and domains are keyed by var name
    :constraints  (hash-table)
    ; domains becomes a hash on init
    :domains      #f
    )))    

    (define (get-var var-name)
      (_ :assigned var-name))

    (define (add-constraint c-sym vars)
      ; add reference to the constraint for each var it is against
      (dolist (v vars)
        (let ((var-constraints (_ :constraints v)))
          (if (not (member? c-sym var-constraints))
            (set! (_ :constraints v) (cons c-sym (_ :constraints v)))))))

    ; XXX incomplete
    (define (apply-constraints var)
      (post "(csp::apply-constraints) var:" var)
      ; get the constraints for a var
      (dolist (c-sym (_ :constraints var))
        (post "applying constraint" c-sym))
      (post "  - done applying constraints")
    )

    ; XXX incomplete
    (define (check-assign var val)
      "attempt to assign a value to a var, checking constraints
       returns false if invalid, returns assigned val if valid"
      (let ((constraint-preds (_ :constraints var)))
        (post "check-assign, c's:" constraint-preds)
        ; LEFT OFF, how to filter through chain of preds?
      ))

    (define (print)
      (post _))

    ; explicit init for setting the preassinged context vars
    (define (init tonic tonality root quality)
      (post "(csp::init)" tonic tonality root quality)
      ; for the preassigned values, domain is reduced to one value already
      (set! (_ :domains) 
        (hash-table 'tonic tonic 'tonality tonality 'root root 'quality quality
          0 note-domain-values 
          1 note-domain-values 
          2 note-domain-values 
          3 note-domain-values))
      ; for each var, initialize a list to hold the constraints
      (dolist (var (_ :vars))
        (set! (_ :constraints var) '()))
      (post " - csp initialized"))


    (define (get . args) 
      (apply _ args))

    ; generic dispatch so any internal function can be called as (csp 'fun args ...)
    (lambda (msg . args)
      (apply (eval msg) args))))


(define csp (make-csp))
(csp 'init 'C 'Major 'I 'Maj7)
(csp 'add-constraint 'first-note-tonic? '(tonic 0))
(csp 'add-constraint 'above-first-octave? '(0))

;********************************************************************************

; variables to fill - notes is a vector of 4 notes
(define notes (make-vector 4 #f))

; naive version that just returns the next empty slot in the variables vector
(define (select-var csp notes)
  "return index of next unassigned variable"
  (post "(select-var) notes:" notes)
  (let ((next-slot #f)
        (i 0))
    (while (< i (length notes))
      (if (false? (notes i)) 
        (begin (set! next-slot i) (break)))
      (set! i (+ 1 i)))
    (post "  - select-var returning" next-slot)
    next-slot))  


; bts-4, does pre and post assignment constraint checking
; recurse down until notes vector full, using range 0-4 and only passes on 3
; 
(define (bts-4 csp notes)
  (define (bts csp notes depth)
    ;if assignment complete, we are done return assignment
    (post "(bts) depth:" depth "notes:" notes)
    (cond 
      ; case done, vector of 4 notes filled, return the filled notes
      ((or (false? notes) (assigned? notes))
         notes)
      (else
        ; if we have filled it, it will bounce back up the stack
        (let ((var-i (select-var csp notes)))
          ; iterate through domain values for i
          (let* rec-loop ((vals (get-domain-values csp var-i notes)))
            ;(post "rec-loop: domain-vals:" vals)
            ; test first value, if good, use it and recurse
            (cond
              ((null? vals)
                (post " - no passing domain value found, return failure")
                #f)
              ((passes-pre-check? csp var-i notes (first vals))
                ;(post " - found passing domain value, using and recursing:")
                ; value good, use it and recurse
                (post " - setting note, var-i" var-i "val" (first vals))
                (set! (notes var-i) (first vals))
                (if (passes-post-check? csp var-i notes)
                  (bts csp notes (+ 1 depth))
                  ; else post check failed, unset last value and return failure
                  (begin 
                    (set! (notes var-i) #f)
                    #f)))
              ; else on to next domain value  
              (else 
                (rec-loop (cdr vals)))))))))
  ; kick it off
  (bts csp notes 0))          


;(load-from-max "csp-1-tests.scm")
;(run-tests)

(post "csp-1.scm loaded")


