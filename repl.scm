
(csp 'apply-constraints 0)

(csp 'get :domains 0)

(csp 'get :constraints)

(csp 'check-constraints 0 '(C . 1))
(csp 'check-constraints 0 '(C . 0))
(csp 'check-constraints 0 '(D . 1))

(begin
(csp 'assign-if-valid 0 '(D . 1))
(post "ASSIGNS:" (csp 'get :assignments))
(post "")
(csp 'assign-if-valid 0 '(C . 1))
(post "ASSIGNS:" (csp 'get :assignments))
)

(csp 'get :constraints-for-var)

(csp 'get-applicable-constraints 0)

(begin
(csp 'assign 0 'C)
(csp 'assign 1 'C)
(csp 'assign 2 'C)
(csp 'assign 3 'C)
(csp 'select-var)
(csp 'all-notes-assigned?)
)

(csp 'all-notes-assigned?)
(csp 'all-notes-assigned?)
(csp 'get-domain-values 0)

(csp 'solve)

(is-chord-factor? csp 0 'C0 0)

(csp 'get-var 'tonic)
(append '(1 2) '(3 4))
