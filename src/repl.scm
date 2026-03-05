(post :hello)



(st 'make-child-id n)

(define n2 (st 'add-node n (hash-table :domain-values '(a b c))))
(n2 :id)

(set! ((rootlet) 'a) 1)

(varlet (rootlet) 'a 1)

st
(dolist (c (str :children)) (st 'repr c))

(format #f "~{~a|~}" (list 1 2 3))
