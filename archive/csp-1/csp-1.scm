; csp-1 a brute force backtracking search to arpeggiate chords

(post "csp-1 loading")
(load-from-max "stuff.scm")
(define (s4m-reset) (delay 100 (lambda()(send 'reset 'bang))))

(define chord-defs (hash-table
  'C7 (vector 0 4 7 10)))

;*********************************************************************************
; some helpers
(define (false? x) 
  (eq? #f x))
(define (not-false? x) 
  (not (false? x)))

(define (assigned? v)
  "check if a vector has a false value anywhere"
  (define (rec-assigned v i)
    (cond 
      ((>= i (length v)) #t)
      ((false? (v i)) #f)
      (else (rec-assigned v (+ 1 i)))))
  (rec-assigned v 0))

; make a tree object that can return values recursing up the tree from a leaf
(define (make-tree)
  (let ((depth 0)
        (root (hash-table :children '() :depth 0 :value #f))
       )
    (define (add-node parent value)
      (let* ((parent-node (if parent parent root))
             (node-depth (+ 1 (parent-node :depth)))
             (node (hash-table 
                     :children '() 
                     :depth node-depth 
                     :parent parent-node
                     :value value)))
        (set! (parent-node :children) (cons node (parent-node :children)))
        (if (> node-depth depth) (set! depth node-depth))
        node))

    (define (print)
      (post "tree depth:" depth "structure" root))

    (define (values-from-node node)
      ;(post "values-from-node, starting at depth:" (node :depth))
      ; recurse up the tree from a node to get list of values
      (let* rec-loop ((n node) 
                      (vals '()))
        (cond 
          ((eq? #f (n :parent))
            vals)
          (else
            (rec-loop (n :parent) (cons (n :value) vals))))))

    (lambda (msg . args)
      (apply (eval msg) args))) ; end let
)

;*********************************************************************************
; predicates used for our constraints
(define (in-chord? note-num chord-sym)
  (let ((chord-factors (chord-defs chord-sym)))
    (if (and chord-factors (member? note-num chord-factors)) #t #f)))

(define (chord-root? note-num chord-sym)
  (let ((chord-factors (chord-defs chord-sym)))
    (and chord-factors (eq? note-num (chord-factors 0)))))


;********************************************************************************
; main prog

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

(define (passes csp notes)
  #t)

; must return a list
(define (get-domain-values csp var notes)
  "return our dummy range of notes, from 0 to 12"
  (range 0 4))

(define (passes-pre-check? csp var notes val)
  (cond 
    ((= var 0)
      (= 2 val))
    ((= var 1)
      (= 3 val))
    (else
      (= 5 val))))

(define (passes-post-check? csp var notes)
  ; check validity of notes array
  #f)

; bts-4, does pre and post assignment constraint checking
; recurse down until notes vector full, using range 0-4 and only passes on 3
; needs to iterate through possible domain values, only using good one
; if the value passes, we recurse, if it doesn't, we iterate to next value
; if we cant find a solution, notes is returned as #f
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


