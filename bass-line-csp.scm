; csp-1 a brute force backtracking search to arpeggiate chords
(post "csp-1 loading")
(load-from-max "stuff.scm")
(load-from-max "bcsp-helpers.scm")
(load-from-max "bcsp-constraints.scm")
(define (s4m-reset) (delay 100 (lambda()(send 'reset 'bang))))


;*********************************************************************************
; message-based csp object
; note that this implementation means #f CANNOT be a valid domain value!
(define (make-csp) 
  (let ((self #f) ; gets set in init
        (_ (hash-table 
            ; variable names, numbers correspond to notes
            :vars         '(tonic tonality root quality 0 1 2 3)   
            :context-vars '(tonic tonality root quality)
            :note-vars    '(0 1 2 3)
            ; assigned vals is a hash-table keyed by var name (or number)
            :assignments  (hash-table)  
            ; constraints and domains are keyed by var name
            :constraints  (hash-table)
            ; domains becomes a hash on init
            :domains      #f
            )))    

    (define (get-var var-name)
      (_ :assignments var-name))

    (define (add-constraint c-sym vars)
      ; add reference to the constraint for each var it is against
      (dolist (v vars)
        (let ((var-constraints (_ :constraints v)))
          (if (not (member? c-sym var-constraints))
            (set! (_ :constraints v) (cons c-sym (_ :constraints v)))))))

    (define (check-constraints var val)
       "apply all constraints for a var, returning value if success, false otherwise"
       (post "(check-constraints) var:" var "val:" val "c's:" (_ :constraints var))
       (let test-loop ((v val) (cp-list (_ :constraints var)))
         (cond 
           ((null? cp-list) ; got through list, return value 
             v)
           ; case getting and testing pred passes, on to next
           ((let* ((cp-sym (car cp-list))
                   (cp-fun (eval cp-sym)))
              (cp-fun self var v))  
                (test-loop v (cdr cp-list)))
           (else             ; testing pred returned false, done
             #f))))

    (define (assign-if-valid var val)
      "assign a value to a variable if it passes constraints
       return false on failure, val on success"
       (let ((checked-val (check-constraints var val)))
         (if checked-val
           (set! (_ :assignments var) checked-val) ; this will return the val too
           #f)))
    
    ; explicit init for setting the pre-assinged vars
    (define (init self-ref pre-assignments)
      (post "(csp::init) pre-assignments:" pre-assignments)
      (set! self self-ref) ; hacky, figure out better way later - macros?
      (set! (_ :domains) 
        (hash-table 
          0 note-domain-values 
          1 note-domain-values 
          2 note-domain-values 
          3 note-domain-values))
      ; set from the starting-assignments
      ; for the preassignments values, domain is also set to list of one value already
      (for-each 
        (lambda (p)
          (set! (_ :assignments (car p)) (cdr p))
          (set! (_ :domains (car p)) (list (cdr p))))
        pre-assignments)
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
(csp 'init csp (hash-table 'tonic 'C  'tonality 'Major  'root 'I 'quality 'Maj7))
(csp 'add-constraint 'is-tonic? '(tonic 0))
(csp 'add-constraint 'above-oct-0? '(0))

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


