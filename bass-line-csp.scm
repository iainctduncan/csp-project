; csp-1 a brute force backtracking search to arpeggiate chords
(post "csp-1 loading")
(load-from-max "stuff.scm")
(load-from-max "bcsp-helpers.scm")
(load-from-max "bcsp-constraints.scm")
(define (s4m-reset) (delay 100 (lambda()(send 'reset 'bang))))


;*********************************************************************************
; message-based csp object
; note that this implementation means #f CANNOT be a valid domain value!
(define (make-csp num-notes) 
  (let ((self #f) ; gets set in init
        (_ (hash-table 
            ; variable names, numbers correspond to notes
            :vars         '(tonic tonality root quality 0 1 2 3)   
            :context-vars '(tonic tonality root quality)
            :note-vars    '(0 1 2 3)
            ; assigned vals is a hash-table keyed by var name (or number)
            :ctx-assignments  (hash-table)  
            :note-assignments (make-vector num-notes #f)
            ; domains becomes a hash-table of domain val lists, keyed by var name
            :domains      #f
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
      (set! (_ :domains) 
        (hash-table 
          0 note-domain-values 
          1 note-domain-values 
          2 note-domain-values 
          3 note-domain-values))
      (pre-assign pre-assignments)
      ; set from the starting-assignments
      ; for each var, initialize a list to hold the constraints
      (dolist (var (_ :vars))
        (set! (_ :constraints var) '()))
      (post " - csp initialized"))

    (define (pre-assign pre-assignments-ht)
      "pre-assign a value to a var, also reducing the vars domain"
      (post "(csp::pre-assign) ht: " pre-assignments-ht)
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

    (define (get-var var)
      (if (number? var)
        (_ :note-assignments var)
        (_ :ctx-assignments var)))

    (define (add-constraint pred vars . args)
      "add constraint to the two constraint registries"
      (post "add-constraint, pred:" pred "vars:" vars)
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

    ; busted here! LEFT OFF:
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
      
      ; this works with the object vals, not so sure if that is better than having it separate
      (define (search result depth)
        ;if assignment complete, we are done return assignment
        ;(post "")
        ;(post "csp::solve::(search) depth:" depth "notes:" (_ :note-assignments))
        (cond 
          ; case done, no solution found, return failure
          ((false? result)
            #f)
          ; case done, vector of 4 notes filled, return success
          ((all-notes-assigned?)
             #t)
          ; else we still have notes to fill
          (else
            ; get the next far to fill
            (let ((var (select-var)))
              ; iterate through domain values for i
              (let* rec-loop ((vals (get-domain-values var)))
                ;(post "rec-loop: domain-vals:" vals)
                ; test first value, if good, use it and recurse
                (if (null? vals)
                  ; case ran out of domain vals, return failure back up
                  (begin
                    ;(post " - no passing domain value found, return failure up stack")
                    #f)
                  ; else, try assigning the val
                  (let ((passed (assign-if-valid var (first vals))))
                    (cond
                      ; didn't pass precheck, on to next possible domain value
                      ((not passed) 
                        (rec-loop (cdr vals)))
                      ; passed precheck, failed globals: unset and continue domain val loop 
                      ((not (check-global-constraints))
                        (set! (_ :note-assignments var) #f)
                        (rec-loop (cdr vals)))
                      ; passed everything, found value, recurse onwards
                      (else
                        (post " - found passing domain val:" passed "for depth" depth "recursing")
                        (search passed (+ 1 depth)))))))))))
                  
      ; kick it off, using passing in a ref to the assignements vector, which will get filled
      (let* ((result (search #t 0)))
        (cond
          (result
            (post "solved, notes:" (_ :note-assignments))
            result)
          (else
            (post "no solution found, returning false")
            #f))))

    (define (get . args) 
      (apply _ args))

    ; generic dispatch so any internal function can be called as (csp 'fun args ...)
    (lambda (msg . args)
      (apply (eval msg) args))))


;********************************************************************************
(define csp (make-csp 4))
(csp 'init csp (hash-table 'tonic 'C  'tonality 'Major  'root 'I 'quality 'Maj7))

(csp 'add-constraint is-tonic? '(tonic 0) 'is-tonic)
(csp 'add-constraint above-oct-0? '(0) 'above-oct)
(csp 'add-global-constraint all-diff?)



;(load-from-max "csp-1-tests.scm")
;(run-tests)

(post "csp-1.scm loaded")


