

(begin
(define csp (make-csp 5))
(csp 'init csp (hash-table 
                'tonic 'C  'tonality 'Major  
                'root 'I  'quality 'Dom7
                'target 'IV 'target-q 'Dom7))

(csp 'add-constraint is-tonic? '(tonic 0) 'is-tonic)
(csp 'add-constraint above-oct-0? '(0) 'above-oct)
(csp 'add-constraint chord-root? '(0)  'n0-root)
(csp 'add-constraint in-chord? '(1) 'in-chord-1)
(csp 'add-constraint in-chord? '(2) 'in-chord-2)
(csp 'add-constraint target-root? '(4) 'target-root)

(csp 'add-global-constraint all-diff?)
(csp 'add-global-constraint (intv-under? 'maj-3))
(csp 'add-global-constraint target-from-neighbour?)
)



(root-chord->pitches (csp 'get-var 'root) (csp 'get-var 'quality))

(csp 'assign-if-valid 0 'C1)

(in? 5 '(1 2 3))
