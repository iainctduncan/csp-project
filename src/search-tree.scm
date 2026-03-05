(post "search-tree.scm")
(load-from-max "search-tree-test.scm")

; new version of tree intended to make persistent tree of domain values and found values

; make a tree object that can return values recursing up the tree from a leaf
(define (make-search-tree . args)

  (define (make-id-iterator)
      "return an anonymous closure for counting up"
      (let ((count 0))
        (lambda()(set! count (+ 1 count)))))

  (let ((defaults (if (null? args) (hash-table) (args 0)))
        (depth 0)
        (root (hash-table 
                :depth 0 
                :children '()
                :id-iterator (make-id-iterator) ; for making child ids
                :id "root")))
  
    ; constructor logic, set optional attrs on root
    (for-each (lambda(kv)(set! (root (car kv)) (cdr kv))) defaults)
    
    (define (make-child-id node)
      (let ((new-int (apply (node :id-iterator))))
        (string-append (node :id) "-" (number->string new-int))))

    ; nodes are hashtables, and are returned by add node
    ; can be optionally passed in a hash to COPY from
    (define (add-node parent . args)
      (post "***** tree (add-note) args:" args)
      (let* ((parent-node (if parent parent root))
             (node-depth (+ 1 (parent-node :depth)))
             (node (hash-table 
                     :parent parent-node
                     :id (make-child-id parent-node)
                     :depth node-depth 
                     :children '() 
                     :domain-values '()
                     :id-iterator (make-id-iterator) ; index making fun
                     )))
                     
        ; if passed in a hash-table, copy its contents to the node
        ; this is busted, not sure why
        ;(if (and (length args) (hash-table? (args 0)))
        ;  (for-each
        ;    (lambda (p)(set! (node (car p)) (cdr p)))
        ;    (args 0)))

        ; update the parent nodes child list
        (set! (parent-node :children) (cons node (parent-node :children)))
        (if (> node-depth depth) (set! depth node-depth))
        node))

    ; args 0 is option sequence of domain vals
    (define (init-node-domain node . args)
      ; either set up value list from optional arg 1 or use preexisting
      (let ((domain-vals (if (> (length args) 0) (args 0) (node :domain-values)))
            (domain-val-weight 1)
            )
          ; from the list, make a domain hash-table of weights
          ; they get set to #f when they fail
          (set! (node :domain-ht) (hash-table))
          (dolist (dv domain-vals)
            (set! (node :domain-ht dv) domain-val-weight))
          (post "(init-node-domain) domain-ht:" (node :domain-ht))
          node))

    (define (get-domain-value node)
      "return a candidate domain value for a node or false if no options"
      ; make a flat list from the domain values ht for the node
      (let ((candidate-list (list)))
        (for-each (lambda (kv)
          (let ((val (car kv)) (weight (cdr kv)))
            ; if this domain value has not been rejected, put in the flat list
            (if (not-false? val)
              (set! candidate-list (append candidate-list (make-list weight val))))))
           (node :domain-ht))
        (post "- candidate list:" candidate-list)
        (cond 
          ; if the candidate list is empty, return false (out of domain values)
          ((null? candidate-list) #f)
          ; else choose one randomly, and set it to false to indicate it was used
          (else
            (let* ((index (random (length candidate-list)))
                   (value (candidate-list index)))
              (post "- returning value:" value)
              ; set to false to use it up
              (set! (node :domain-ht value) #f)
              value)))))

    (define (prune-domain-value node value)
      "mark a domain value as having failed for a given node"
      (set! (node :domain-ht value) #f))

    ;(define (repr node)
    ;  "return string rep of node by recursing up parent"

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

    (define (get-root)
      root)

    (lambda (msg . args)
      (apply (eval msg) args))) ; end let
)

