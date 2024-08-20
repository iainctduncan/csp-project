


(define (pop-random v-list)
  (let* ((index (random (length v-list)))
         (value (v-list index)))
    (set! v-list
      (let rloop ((i 0) (front '()) (rest v-list))
        (cond
          ((= i index)
            ; return list without the popped item
            (append (reverse front) (cdr rest)))
          (else
            (rloop (+ 1 i) (cons (car rest) front) (cdr rest))))))
    value))          

(define-macro (pop-random! list-sym)
  (let-set! (outlet (curlet)) list-sym 
    (let* ((v-list (eval list-sym))
           (index (random (length v-list)))
           (value (v-list index)))
      (let rloop ((i 0) (front '()) (rest v-list))
        (cond
          ((= i index)
            ; return list without the popped item
            (append (reverse front) (cddr rest)))
          (else
            (rloop (+ 1 i) (cons (car rest) front) (cdr rest)))))))
    )  

(define (remove-value value val-list)
  (let rloop ((i 0) (front '()) (rest val-list))
    (cond
      ((eq? (car rest) value)
        ; return list without the popped item
        (append (reverse front) (cdr rest)))
      (else
        (rloop (+ 1 i) (cons (car rest) front) (cdr rest)))))) 

(remove-value 2 '(1 2 3 4))

(begin
(define l (list 1 2 3 4))
;(list-extend l)
(post (pop-random! l))
(post l)
)

