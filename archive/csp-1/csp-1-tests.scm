
(define (run-tests)
  (if (not (and
    
    (in-chord? 4 'C7)
    (not (in-chord? 3 'C7))
    (chord-root? 0 'C7) 
   
    (assigned? (vector 1 2 3))
    (not (assigned? (vector 1 #f 3)))
    (not (assigned? (vector #f #f #f #f)))

    (eq? 2 (select-var #f (vector 0 1 #f 3)))
    (eq? 1 (select-var #f (vector 0 #f #f 3)))
    (eq? #f (select-var #f (vector 0 1 2 3)))

    ))
    (post "FAILED")))

(define (tree-test)
  (define tree (make-tree))
  (define node-1 (tree 'add-node #f :a))
  (tree 'print)
)

(post "csp-1-tests.scm loaded")
