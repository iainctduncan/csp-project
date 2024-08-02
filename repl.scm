
(csp 'apply-constraints 0)

;'(C . 0)
(csp 'get :domains 0)

(define vals (hash-table :a 1))
(apply vals '(:a))

(csp 'get :constraints 0)

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
