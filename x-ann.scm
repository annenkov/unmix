(ux:install-macro
  'rcall
  (lambda (x)
    (cond ((syntax-match? '(rcall) '(rcall exp) x) (cadr x))
          (else (error "rcall: invalid syntax " x)))))

(ux:install-macro
  'generalize
  (lambda (x)
    (cond ((syntax-match? '(generalize) '(generalize exp) x) (cadr x))
          (else (error "generalize: invalid syntax " x)))))

(ux:install-macro
  'when
  (lambda (x)
    (cond ((syntax-match? '(when) '(when pred exp1 exp2 ...) x)
           `(if ,(cadr x) (begin unquote (cddr x))))
          (else (error "when: invalid syntax " x)))))

