; make this smarter later
(define (make-sym arg-1 arg-2)
  (symbol (format #f "~a~a" arg-1 arg-2)))

(define *pitch-classes*
  '(C Db D Eb E F Gb G Ab A Bb B))
 
(define *octaves* (range 0 5))

(define (make-note-domain-values pitch-classes octaves)
  "return a list of all possible note domain values, from octave 0 to 3 (48 notes)"
  (let ((vals '()))
    (for-each (lambda (o)
      (for-each (lambda (p)
        (set! vals (cons (cons p o) vals)))
        pitch-classes))
      octaves)
    (reverse vals)))  

(define note-domain-values
  (make-note-domain-values *pitch-classes* *octaves*))

(define (false? x) 
  (eq? #f x))

(define (not-false? x) 
  (not (false? x)))


; make a tree object that can return values recursing up the tree from a leaf
(define (make-tree)
  (let ((depth 0)
        (root (hash-table :children '() :depth 0 :value #f))
       )
    (define (add-node parent value)
      (let* ((parent-node (if parent parent root))
             (node-depth (+ 1 (parent-node :depth)))
             (node (hash-table 
                     :children '() 
                     :depth node-depth 
                     :parent parent-node
                     :value value)))
        (set! (parent-node :children) (cons node (parent-node :children)))
        (if (> node-depth depth) (set! depth node-depth))
        node))

    (define (print)
      (post "tree depth:" depth "structure" root))

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

    (lambda (msg . args)
      (apply (eval msg) args))) ; end let
)


