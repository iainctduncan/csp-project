(define (st-test)
  (begin
    (define st (make-search-tree (hash-table :id "foo")))
    (st 'get-root)
    (define root   (st 'add-node #f (hash-table :domain-values '(a b c))))
    (define root-1 (st 'add-node root (hash-table :domain-values '(a b c))))
    (define root-2 (st 'add-node root (hash-table :domain-values '(a b c))))
    (define root-2-1 (st 'add-node root-2 (hash-table :domain-values '(a b c))))
    ; call to build the domain value structure
    ;(st 'init-node-domain n)
    ;(st 'get-random-domain-value n)
    (post (root :id))
    (post (root-1 :id))
    (post (root-2 :id))
    (post (root-2-1 :id))
  )
)



