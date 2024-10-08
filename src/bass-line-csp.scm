; csp-1 a brute force backtracking search to arpeggiate chords
(post "csp-1 loading")
(load-from-max "stuff.scm")
(load-from-max "bcsp-helpers.scm")
(load-from-max "bcsp-constraints.scm")
(define (s4m-reset) (delay 100 (lambda()(send 'reset 'bang))))

; readability helpers
(define (false? x) 
  (eq? #f x))

(define (not-false? x) 
  (not (false? x)))

(define (make-sym arg-1 arg-2)
  (symbol (format #f "~a~a" arg-1 arg-2)))

; for debugging access
(define __ #f)
;*********************************************************************************
; message-based csp object
; note that this implementation means #f CANNOT be a valid domain value!
(define (make-csp num-notes) 
  (let ((self #f) ; gets set in init
        (_ (hash-table 
            ; note, vars and note-vars get extended/set by list of note indices in init
            :vars         '(tonic tonality root quality target target-q)   
            ;:vars         '(tonic tonality root quality 0 1 2 3 4)
            :context-vars '(tonic tonality root quality)
            ;:note-vars    '(0 1 2 3 4) ; will become '(0 ... num-notes) 
            :note-vars    '() ; will become '(0 ... num-notes) 
            ; assigned vals is a hash-table keyed by var name (or number)
            :ctx-assignments  (hash-table)  
            :note-assignments (make-vector num-notes #f)
            ; domains becomes a hash-table of domain val lists, keyed by var name
            :domains      (hash-table)
            ; constraint registry, keyed by a gensym id
            ; each entry is hash of pred, vars, id, applied
            ; we can stash data we need for rewinding there too maybe?
            :constraints  (hash-table)
            :g-constraints '()
            ; registry of ids by var, keys are the var token, vals the list of constraints
            :constraints-for-var  (hash-table)
            )))    

    ; explicit init for setting the pre-assinged vars
    (define (init self-ref pre-assignments)
      (post "(csp::init) pre-assignments:" pre-assignments)
      (set! self self-ref) ; hacky, figure out better way later - macros?
      (set! (_ :note-vars) (range 0 num-notes))
      (set! (_ :vars) (append (_ :vars) (range 0 num-notes)))
      ; domains are vectors of symbols of notes
      (dolist (i (_ :note-vars))
        (set! (_ :domains i) (vector->list note-symbols)))
      ; reset notes to false
      (dotimes (i num-notes) (set! (_ :note-assignments i) #f))
      (pre-assign pre-assignments)
      ; set from the starting-assignments
      ; for each var, initialize a list to hold the constraints
      (dolist (var (_ :vars))
        (set! (_ :constraints var) '()))
      ; for debugging, save state to top level
      (set! __ _)
      (post " - csp initialized"))

    (define (pre-assign pre-assignments-ht)
      "pre-assign a value to a var, also reducing the vars domain"
      ;(post "(csp::pre-assign) ht: " pre-assignments-ht)
      ; for the preassignments values, domain is also set to list of one value already
      (for-each 
        (lambda (p)
          (let ((var (car p)) 
                (val (cdr p)))
            (if (number? var)
              (set! (_ :note-assignments var) val)
              (set! (_ :ctx-assignments var) val))
          (set! (_ :domains var) (list val))))
        pre-assignments-ht))

    (define (get-domain-values var)
      ; TODO add some shuffling so we get a new result on each search 
      (_ :domains var))

    (define (get-random-domain-value var)
      "return a random value from a var's domain val list"
      ((_ :domains var) (random (length (_ :domains var)))))

    (define (remove-domain-value var value)
      "remove a domain value from a vars domain values
       assumes a value will only be in the domain values list once"
      (set! (_ :domains var) 
        (let rloop ((i 0) (front '()) (rest val-list))
          (cond
            ((null? rest)
              (reverse front))
            ((eq? (car rest) value)
              ; return list without the popped item
              (append (reverse front) (cdr rest)))
            (else
              (rloop (+ 1 i) (cons (car rest) front) (cdr rest)))))))

    (define (get-var var)
      (if (number? var)
        (_ :note-assignments var)
        (_ :ctx-assignments var)))

    (define (add-constraint pred vars . args)
      "add constraint to the two constraint registries"
      ;(post "add-constraint, pred:" pred "vars:" vars)
      ; add an ht record for the constraint keyed by gensym
      (let* ((c-id (if (length args) (args 0) gensym))
             (c-entry (hash-table
                        :id         c-id
                        :predicate  pred  ; could be symbol or fun
                        :vars       vars
                        :applied    #f)))
        (set! (_ :constraints c-id) c-entry)
        ; now add the constraint id to the registry keyed by vars
        (dolist (v vars)
          (let ((var-constraints (_ :constraints-for-var v)))
            ;(post "  - var:" v "var-constraints:" var-constraints)
            (cond
              ; case first entry for this var: add initial list
              ((or (not var-constraints) (null? var-constraints))  
                ;(post "    - adding new list for var" v)
                (set! (_ :constraints-for-var v) (list c-id)))
              ; case var has entry, but this constraint not in it, add to list
              ((not (member? c-id var-constraints))
                ;(post "    - adding to existing c list for var" v)
                (set! (_ :constraints-for-var v) (cons c-id (_ :constraints-for-var v))))
              ; else nothing to do, constraint already in list for var
              (else
                ;(post " already there, noop")
                (begin)))))
        ;(post "   added id" c-id ":constraints-for-var now:" (_ :constraints-for-var))
      ))

    ; for now, global constraints are just a list
    (define (add-global-constraint pred)
      (set! (_ :g-constraints) (cons pred (_ :g-constraints))))

    (define (get-applicable-constraints var)
      "return all constraint ids for a var that don't depend on unassigned vars (excluding this var)"
      (let* ((all-ids-for-var (_ :constraints-for-var var)))
        ;(post "get-applicable-constraints var:" var "all-ids-for-var:" all-ids-for-var)
        ; return true if constraint is not over unset vars 
        (define (is-applicable? c-id)
          ; loop through the variables this constraint is over
          (let rec-loop ((vars-over (_ :constraints c-id :vars)))
            (cond
              ((null? vars-over)  
                #t)
              ; if the var and the next in the loop aren't the same
              ; and the var in the loop has not been set, this is not applicable
              ((let ((test-var (car vars-over))
                     (asg-store (if (number? vars-over) (_ :note-assignments) (_ :ctx-assignments))))
                (and (not (eq? var test-var)) 
                     (not (asg-store test-var)))) 
                #f)
              (else 
                (rec-loop (cdr vars-over))))))
        (if all-ids-for-var
          (filter is-applicable? all-ids-for-var)
          '())))

    ; fix up check constraints to use the above
    (define (check-constraints var val)
       "apply all applicable constraints for a var, returning value if success, false otherwise"
       ;(post "(check-constraints) var:" var "val:" val)
       (let* ((c-ids   (get-applicable-constraints var))
              (c-preds (map (lambda (id)(_ :constraints id :predicate)) c-ids)))
         ;(post "  - constraint ids:" c-ids "c-preds:" c-preds)
         (let test-loop ((v val) (cp-list c-preds))
           (cond 
             ((null? cp-list) ; got through list ok, return value as it passes
               v)
             ; case getting and testing pred passes, recur to next pred
             (((car cp-list) self var v)  
               (test-loop v (cdr cp-list)))
             (else             ; testing pred returned false, done and return false
               #f)))))
   
    (define (check-global-constraints)
      "check the constraints that run all the time, true on success, stop on first fail"
      (let rloop ((preds (_ :g-constraints)))
        (cond
          ((null? preds) 
            #t)
          ((false? ((car preds) self (_ :note-assignments))) 
            #f)
          (else 
            (rloop (cdr preds))))))

    (define (var-assigned? var)
      (if (number? var)
        (if (_ :note-assignments var) #t #f)
        (if (_ :ctx-assignments var) #t #f)))

    (define (all-notes-assigned?)
      "check if a sequence is assigned (has no false values)"
      (let rloop ((i 0))
        (cond
          ((= i (length (_ :note-assignments))) #t)
          ((false? (_ :note-assignments i)) #f)
          (else (rloop (+ 1 i))))))

    (define (assign-if-valid var val)
      "assign a value to a variable if it passes constraints
       return false on failure, val on success"
       (let ((checked-val (check-constraints var val))
             (assignments (if (number? var) (_ :note-assignments) (_ :ctx-assignments))))
         (if checked-val
           (set! (assignments var) checked-val) ; this will return the val too
           #f)))
  
    (define (assign var val)
      "assign a var, skip checking"
      (post "(assign)" var val)
      (if (number? var)
        (set! (_ :note-assignments var) val)
        (set! (_ :ctx-assignments var) val)))

    ; naive version that just returns the next empty slot in the note assignments vector
    (define (select-var)
      "return index of next unassigned note var"
      ;(post "(csp::select-var)")
      (let ((next-slot #f)
            (i 0))
        (while (< i num-notes)
          (if (false? (_ :note-assignments i)) 
            (begin (set! next-slot i) (break)))
          (set! i (+ 1 i)))
        ;(post "  - select-var returning" next-slot)
        next-slot))  

    ; the main search method, fills and returns the notes vars on success
    ; can take optional positional arg of hash-table of preassignments
    (define (solve . args)
      (if (> (length args) 0)
        (pre-assign (args 0)))
      (post "SOLVING")
      (post "csp::(solve) ctx:" (_ :cxt-assignments) "notes:" (_ :note-assignments))
      
      (define (recursive-search depth)
        ;if assignment complete, we are done return assignment
        ;(post "")
        (post "csp::solve::(search) depth:" depth "notes:" (_ :note-assignments))
        (cond 
          ; case done, vector of 4 notes filled, return success
          ; executes when we get to the bottom of recuring down
          ((all-notes-assigned?)
             ;(post "  - all note assigned, returning #t up stack")
             #t)
          ; else we still have notes to fill
          (else
            ; get the next var to fill
            (let ((var (select-var)))
              ; iterate through domain values for i
              (let* domain-val-loop ((vals (get-domain-values var)))
                ;(post "domain-val-loop: domain-vals:" vals)
                (if (null? vals)
                  ; case ran out of domain vals, return failure back up
                  (begin
                    ;(post " - out of possible domain values, return #f up stack")
                    #f)
                  ; else, try assigning the val, check constraints we can run so far
                  (let ((passed (assign-if-valid var (first vals))))
                    (cond
                      ; didn't pass precheck, on to next possible domain value
                      ((not passed) 
                        (domain-val-loop (cdr vals)))
                      ; passed precheck, failed globals: unset and continue domain val loop 
                      ((not (check-global-constraints))
                        (set! (_ :note-assignments var) #f)
                        (domain-val-loop (cdr vals)))
                      ; passed everything, found value, recurse onwards
                      ; if recursing fails, unset var and continue looking
                      ((not (recursive-search (+ 1 depth)))
                        (set! (_ :note-assignments var) #f)
                        (domain-val-loop (cdr vals)))
                      (else
                        ;(post " - found passing domain val:" passed "for depth" depth "returning #t")
                        #t)))))))))
                  
      ; kick it off, using passing in a ref to the assignements vector, which will get filled
      (let* ((result (recursive-search 0)))
        (cond
          (result
            (post "SOLVED, notes:" (_ :note-assignments))
            (_ :note-assignments))
          (else
            (post "no solution found, returning false")
            #f))))

    (define (init-solve args)
      (init self args)
      (post "(init-solve) ctx:" (_ :ctx-assignments) "notes:" (_ :note-assignments))
      (solve))

    (define (get . args) 
      (apply _ args))

    ; generic dispatch so any internal function can be called as (csp 'fun args ...)
    (lambda (msg . args)
      (apply (eval msg) args))))


;********************************************************************************

;(load-from-max "csp-1-tests.scm")
;(run-tests)

(post "csp-1.scm loaded")

(define chord-prog '((II Min7) (V Dom7) (I Maj7) (I Maj7)))

(define (add-constraints csp)
  ;(csp 'add-constraint is-tonic? '(tonic 0) 'is-tonic)
  (csp 'add-constraint chord-root? '(0)  'n0-root)
  (csp 'add-constraint in-chord? '(1) 'in-chord-1)
  (csp 'add-constraint in-chord? '(2) 'in-chord-2)
  (csp 'add-constraint target-root? '(4) 'target-root)
  (csp 'add-global-constraint (diff? '(0 1 2)))
  (csp 'add-global-constraint (intv-under? 'maj-3))
  (csp 'add-global-constraint target-from-cn?)
)  

(define csp (make-csp 5))

; function to build four per bar bass line over a prog
(define (bass-line chord-prog tonic tonality)
  (post "(bass-line)" tonic tonality chord-prog)

  (let* (;(csp (make-csp 5))
         (csp-base (hash-table 'tonic tonic 'tonality tonality))
         (notes-per-bar 4)
         (num-bars (length chord-prog))
         (start-on 'D1)
         (line (make-vector (* notes-per-bar num-bars) #f))) 
    (add-constraints csp)
   
    ;(dotimes (b-num num-bars)
    (let solve-loop ((b-num 0) (first-note start-on))
      (post "solve-loop line for bar:" (chord-prog b-num) "start on:" first-note )
      (let* ((next-b-num (+ 1 b-num))
             (this-chord (chord-prog b-num))
             (rnum (this-chord 0))
             (qual (this-chord 1))
             (next-chord (if (< next-b-num num-bars) (chord-prog next-b-num) #f))
             (csp-vals (hash-table 'root rnum 'quality qual
                          'target (if next-chord (next-chord 0) #f)))
             (noop (csp 'init csp (append csp-base csp-vals)))
             (noop (csp 'assign-if-valid 0 first-note))
             (notes-out (csp 'solve)))
        (post "  - notes-out" notes-out)
        ; copy over the notes from the solver
        (dotimes (i notes-per-bar)
          (set! (line (+ (* b-num notes-per-bar) i)) (notes-out i)))
        (post "  copied notes, line now: " line)  
        (post "  next-start note:" (notes-out notes-per-bar))
        ; set starting note for next bar
        (if (< b-num (- num-bars 1))
          (solve-loop (+ 1 b-num) (notes-out notes-per-bar)))))

    (post "DONE line:")
    (dotimes (i (length line)) (post (line i)))
    ))          

