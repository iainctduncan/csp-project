; record of incrementally building up recursive search

; recurse down until notes vector full, using random numbers
(define (bts-1 csp notes)
  (define (bts csp notes depth)
    ;if assignment complete, we are done return assignment
    (post "(bts) depth:" depth "notes:" notes)
    (cond 
      ; case done, vector of 4 notes filled, return the filled notes
      ((assigned? notes)
         notes)
      (else
      ; else, fill a note and recurse down
      ; if we have filled it, it will bounce back up the stack
        (let* ((var-i (select-var csp notes))
               (val-i (random 4)))
          (post "  setting note, var-i" var-i "val-i" val-i)
          (set! (notes var-i) val-i)     
          (bts csp notes (+ 1 depth))))))

; bts-2: works as long as there IS a solution
;  not sure if handles failure, don't think so
; recurse down until notes vector full, using range 0-4 and only passes on 3
; needs to iterate through possible domain values, only using good one
; if the value passes, we recurse, if it doesn't, we iterate to next value
; I think this works only if we have a solution to find right now
(define (bts-2 csp notes)
  (define (bts csp notes depth)
    ;if assignment complete, we are done return assignment
    (post "(bts) depth:" depth "notes:" notes)
    (cond 
      ; case done, vector of 4 notes filled, return the filled notes
      ((assigned? notes)
         notes)
      (else
        ; if we have filled it, it will bounce back up the stack
        (let ((var-i (select-var csp notes)))
          ; iterate through domain values for i
          (let* rec-loop ((vals (get-domain-values csp var-i notes)))
            (post "rec-loop: domain-vals:" vals)
            ; test first value, if good, use it and recurse
            (cond
              ((null? vals)
                (post " - no passing domain value found, return failure")
                #f)
              ((passes-pre-check? csp var-i notes (first vals))
                (post " - found passing domain value, using and recursing:")
                ; value good, use it and recurse
                (post " - setting note, var-i" var-i "val" (first vals))
                (set! (notes var-i) (first vals))
                (bts csp notes (+ 1 depth)))
              ; else on to next domain value  
              (else 
                (rec-loop (cdr vals)))))))))
  ; kick it off
  (bts csp notes 0))          


; bts-3, handles failure properly, includes pre-check only
; recurse down until notes vector full, using range 0-4 and only passes on 3
; needs to iterate through possible domain values, only using good one
; if the value passes, we recurse, if it doesn't, we iterate to next value
; if we cant find a solution, notes is returned as #f, which will be final 
; value, leaving the notes vector as partially filled
(define (bts-3 csp notes)
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
                (bts csp notes (+ 1 depth)))
              ; else on to next domain value  
              (else 
                (rec-loop (cdr vals)))))))))
  ; kick it off
  (bts csp notes 0))          


