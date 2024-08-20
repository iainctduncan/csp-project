(note-nums)

(member? 'b '(1 b 3))
(member? 1 '(3 4 5))
(member? #f (vector #f 2 3))
(member? #f (vector 1 2 3))

(in-chord? 4 'C7)
(chord-root? 0 'C7
(chord-defs 'C7)

(assert (eq? 1 2))
(post 1)
(for-each (lambda (v) (post v)) '(1 2 3))
 
(dolist (i '(1 2 3)) (post i))

(dolist (val (domain-values csp var-i notes))

(begin 
  (define tree (make-tree))
  (define node-1 (tree 'add-node #f :a))
  (define node-2 (tree 'add-node node-1 :b)) 
  ;(tree 'print)
  ;(post "node-1 parent:" (node-1 :parent))
  (post "node-2 values" (tree 'values-from-node node-2))
)

(post "hello")

;(post "running tree test")
;(tree-test)

(range 0 4)
(random 3)

notes


(post "result: " (bts-4 #f notes) "notes:" notes)


