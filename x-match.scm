(define (%*gen-let-vars*% args)
  (map (lambda (arg) (if (symbol? arg) arg (ux:gentemp))) args))

(define (%*del-trivial-bindings*% bindings)
  (if (null? bindings)
    '()
    (let ((var (caar bindings)) (val (cadar bindings)) (rest (cdr bindings)))
      (if (eq? var val)
        (%*del-trivial-bindings*% rest)
        (cons (car bindings) (%*del-trivial-bindings*% rest))))))

(define (%*literal?*% x)
  (or (boolean? x) (number? x) (char? x) (string? x) (vector? x)))

(ux:install-macro
  'match
  (lambda (x)
    (cond ((syntax-match? '(match) '(match (arg ...) clause ...) x)
           (let ((%%4 (%*gen-let-vars*% (cadr x))))
             (if (not (syntax-match? '() '(var ...) %%4))
               (error "match: 'with' pattern does not fit the argument"
                      '(var ...)
                      %%4))
             `(*let$* ,(map (lambda (%%3 %%5) `(,%%5 ,%%3)) (cadr x) %%4)
                      (match$* ,%%4 ,(cddr x)))))
          (else (error "match: invalid syntax " x)))))

(ux:install-macro
  'match$*
  (lambda (x)
    (cond ((syntax-match? '(match$*) '(match$* args ()) x)
           `(error "MATCH: no match for" unquote (cadr x)))
          ((syntax-match? '(match$*) '(match$* args (clause . more)) x)
           `(match$ ,(caaddr x)
                    ,(cadr x)
                    ()
                    ()
                    (match$* ,(cadr x) ,(cdaddr x))))
          (else (error "match$*: invalid syntax " x)))))

(ux:install-macro
  'match$
  (lambda (x)
    (cond ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ (=> . exps) () ((id . val) ...) cnds alt)
             x)
           `(if (and unquote (car (cddddr x)))
              (*let$* ,(map (lambda (%%6) `(,(car %%6) ,(cdr %%6))) (cadddr x))
                      unquote
                      (cdadr x))
              unquote
              (cdr (cddddr x))))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ (& guard => . exps) () ((id . val) ...) (cnd ...) alt)
             x)
           `(if (and ,@(car (cddddr x))
                     (*let$* ,(map (lambda (%%8) `(,(car %%8) ,(cdr %%8)))
                                   (cadddr x))
                             ,(cadadr x)))
              (*let$* ,(map (lambda (%%8) `(,(car %%8) ,(cdr %%8))) (cadddr x))
                      unquote
                      (cdr (cddadr x)))
              unquote
              (cdr (cddddr x))))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ (=> . exps) args env cnds alt)
             x)
           (let ((%%9 (error "MATCH: too many arguments" (caddr x)))) `()))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ clause () env cnds alt)
             x)
           (let ((%%10 (error "MATCH: too few arguments" (cadr x)))) `()))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ (_ . pats) (arg . args) env cnds alt)
             x)
           `(match$ ,(cdadr x) ,(cdaddr x) unquote (cdddr x)))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ (() . pats) (arg . args) env (cnd ...) alt)
             x)
           `(match$ ,(cdadr x)
                    ,(cdaddr x)
                    ,(cadddr x)
                    (,@(car (cddddr x)) (null? ,(caaddr x)))
                    unquote
                    (cdr (cddddr x))))
          ((and (syntax-match?
                  '(match$ & => as _ quote quasiquote)
                  '(match$ (pat . pats) (arg . args) env (cnd ...) alt)
                  x)
                (%*literal?*% (caadr x)))
           `(match$ ,(cdadr x)
                    ,(cdaddr x)
                    ,(cadddr x)
                    (,@(car (cddddr x)) (equal? ,(caaddr x) ,(caadr x)))
                    unquote
                    (cdr (cddddr x))))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ (quote . pats) (arg . args) env cnds alt)
             x)
           (let ((%%13 (error "MATCH: 'quote' appears as variable in pattern")))
             `()))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ (quasiquote . pats) (arg . args) env cnds alt)
             x)
           (let ((%%14 (error "MATCH: 'quasiquote' appears as variable in pattern")))
             `()))
          ((and (syntax-match?
                  '(match$ & => as _ quote quasiquote)
                  '(match$ (pat . pats) (arg . args) env cnds alt)
                  x)
                (and (symbol? (caadr x)) (assoc (caadr x) (cadddr x))))
           (let ((%%15 (error "MATCH: duplicate variable in pattern"
                              (caadr x))))
             `()))
          ((and (syntax-match?
                  '(match$ & => as _ quote quasiquote)
                  '(match$ (pat . pats) (arg . args) env cnds alt)
                  x)
                (symbol? (caadr x)))
           `(match$ ,(cdadr x)
                    ,(cdaddr x)
                    ((,(caadr x) unquote (caaddr x)) unquote (cadddr x))
                    unquote
                    (cddddr x)))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ ('e . pats) (arg . args) env (cnd ...) alt)
             x)
           `(match$ ,(cdadr x)
                    ,(cdaddr x)
                    ,(cadddr x)
                    (,@(car (cddddr x)) (equal? ,(caaddr x) ,(caadr x)))
                    unquote
                    (cdr (cddddr x))))
          ((and (syntax-match?
                  '(match$ & => as _ quote quasiquote)
                  '(match$ ((x as pat) . pats) (arg . args) env cnds alt)
                  x)
                (symbol? (caaadr x)))
           `(match$ (,(caaadr x) ,(cadr (cdaadr x)) unquote (cdadr x))
                    (,(caaddr x) unquote (caddr x))
                    unquote
                    (cdddr x)))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ ((x as pat) . pats) (arg . args) env cnds alt)
             x)
           (let ((%%17 (error "MATCH: invalid pattern" (caadr x)))) `()))
          ((syntax-match?
             '(match$ & => as _ quote quasiquote)
             '(match$ ((xp . yp) . pats) (arg . args) env (cnd ...) alt)
             x)
           (let ((%%19 (*extend-syntax-add-car* (caaddr x)))
                 (%%20 (*extend-syntax-add-cdr* (caaddr x))))
             `(match$ (,(caaadr x) ,(cdaadr x) unquote (cdadr x))
                      (,%%19 ,%%20 unquote (cdaddr x))
                      ,(cadddr x)
                      (,@(car (cddddr x)) (pair? ,(caaddr x)))
                      unquote
                      (cdr (cddddr x)))))
          (else (error "match$: invalid syntax " x)))))

(ux:install-macro
  'with
  (lambda (x)
    (cond ((syntax-match? '(with) '(with bindings . body) x)
           `(with% unquote (cdr x)))
          (else (error "with: invalid syntax " x)))))

(ux:install-macro
  'with*
  (lambda (x)
    (cond ((syntax-match? '(with*) '(with* () . body) x)
           `(let unquote (cdr x)))
          ((syntax-match? '(with*) '(with* ((pat arg)) . body) x)
           `(with% unquote (cdr x)))
          ((syntax-match? '(with*) '(with* ((pat arg) . rest) . body) x)
           `(with% (,(caadr x)) (with* ,(cdadr x) unquote (cddr x))))
          (else (error "with*: invalid syntax " x)))))

(ux:install-macro
  'with%
  (lambda (x)
    (cond ((syntax-match? '(with%) '(with% ((pat arg) ...) exp ...) x)
           (let ((%%23 (%*gen-let-vars*% (map cadr (cadr x)))))
             (if (not (syntax-match? '() '(var ...) %%23))
               (error "with%: 'with' pattern does not fit the argument"
                      '(var ...)
                      %%23))
             `(*let$* ,(map (lambda (%%22 %%24) `(,%%24 unquote (cdr %%22)))
                            (cadr x)
                            %%23)
                      (with$ ,(map car (cadr x)) ,%%23 () ,(cddr x)))))
          (else (error "with%: invalid syntax " x)))))

(ux:install-macro
  'with$
  (lambda (x)
    (cond ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ () () ((id . val) ...) (exp ...))
             x)
           `(*let$* ,(map (lambda (%%26) `(,(car %%26) ,(cdr %%26)))
                          (cadddr x))
                    unquote
                    (car (cddddr x))))
          ((and (syntax-match?
                  '(with$ as _ quote quasiquote)
                  '(with$ ((x as pat) . pats) (arg . args) env body)
                  x)
                (symbol? (caaadr x)))
           `(with$ (,(caaadr x) ,(cadr (cdaadr x)) unquote (cdadr x))
                   (,(caaddr x) unquote (caddr x))
                   unquote
                   (cdddr x)))
          ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ ((x as pat) . pats) (arg . args) env body)
             x)
           (let ((%%27 (error "WITH: invalid pattern" (caadr x)))) `()))
          ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ ('e . pats) (arg . args) env body)
             x)
           `(with$ ,(cdadr x) ,(cdaddr x) unquote (cdddr x)))
          ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ (() . pats) (arg . args) env body)
             x)
           `(with$ ,(cdadr x) ,(cdaddr x) unquote (cdddr x)))
          ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ ((xp . yp) . pats) (arg . args) env body)
             x)
           (let ((%%28 (*extend-syntax-add-car* (caaddr x)))
                 (%%29 (*extend-syntax-add-cdr* (caaddr x))))
             `(with$ (,(caaadr x) ,(cdaadr x) unquote (cdadr x))
                     (,%%28 ,%%29 unquote (cdaddr x))
                     unquote
                     (cdddr x))))
          ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ (_ . pats) (arg . args) env body)
             x)
           `(with$ ,(cdadr x) ,(cdaddr x) unquote (cdddr x)))
          ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ (quote . pats) (arg . args) env body)
             x)
           (let ((%%30 (error "WITH: 'quote' appears as variable in pattern")))
             `()))
          ((syntax-match?
             '(with$ as _ quote quasiquote)
             '(with$ (quasiquote . pats) (arg . args) env body)
             x)
           (let ((%%31 (error "WITH: 'quasiquote' appears as variable in pattern")))
             `()))
          ((and (syntax-match?
                  '(with$ as _ quote quasiquote)
                  '(with$ (pat . pats) (arg . args) env body)
                  x)
                (and (symbol? (caadr x)) (assoc (caadr x) (cadddr x))))
           (let ((%%32 (error "WITH: duplicate variable in pattern"
                              (caadr x))))
             `()))
          ((and (syntax-match?
                  '(with$ as _ quote quasiquote)
                  '(with$ (pat . pats) (arg . args) env body)
                  x)
                (symbol? (caadr x)))
           `(with$ ,(cdadr x)
                   ,(cdaddr x)
                   ((,(caadr x) unquote (caaddr x)) unquote (cadddr x))
                   unquote
                   (cddddr x)))
          ((and (syntax-match?
                  '(with$ as _ quote quasiquote)
                  '(with$ (pat . pats) (arg . args) env body)
                  x)
                (%*literal?*% (caadr x)))
           `(with$ ,(cdadr x) ,(cdaddr x) unquote (cdddr x)))
          (else (error "with$: invalid syntax " x)))))

(ux:install-macro
  'select
  (lambda (x)
    (cond ((syntax-match? '(select) '(select (arg ...) clause ...) x)
           (let ((%%35 (%*gen-let-vars*% (cadr x))))
             (if (not (syntax-match? '() '(var ...) %%35))
               (error "select: 'with' pattern does not fit the argument"
                      '(var ...)
                      %%35))
             `(*let$* ,(map (lambda (%%34 %%36) `(,%%36 ,%%34)) (cadr x) %%35)
                      (select$* ,%%35 ,(cddr x)))))
          (else (error "select: invalid syntax " x)))))

(ux:install-macro
  'select$*
  (lambda (x)
    (cond ((syntax-match? '(select$*) '(select$* args ()) x)
           `(error "SELECT: no match for" unquote (cadr x)))
          ((syntax-match? '(select$*) '(select$* args (clause . more)) x)
           `(select$ ,(caaddr x)
                     ,(cadr x)
                     ()
                     ()
                     (select$* ,(cadr x) ,(cdaddr x))))
          (else (error "select$*: invalid syntax " x)))))

(ux:install-macro
  'select$
  (lambda (x)
    (cond ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ (=> . exps) () ((id . val) ...) cnds alt)
             x)
           `(if (and unquote (car (cddddr x)))
              (*let$* ,(map (lambda (%%37) `(,(car %%37) ,(cdr %%37)))
                            (cadddr x))
                      unquote
                      (cdadr x))
              unquote
              (cdr (cddddr x))))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ (& guard => . exps) () ((id . val) ...) (cnd ...) alt)
             x)
           `(if (and ,@(car (cddddr x))
                     (*let$* ,(map (lambda (%%39) `(,(car %%39) ,(cdr %%39)))
                                   (cadddr x))
                             ,(cadadr x)))
              (*let$* ,(map (lambda (%%39) `(,(car %%39) ,(cdr %%39)))
                            (cadddr x))
                      unquote
                      (cdr (cddadr x)))
              unquote
              (cdr (cddddr x))))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ (=> . exps) args env cnds alt)
             x)
           (let ((%%40 (error "SELECT: too many arguments" (caddr x)))) `()))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ clause () env cnds alt)
             x)
           (let ((%%41 (error "SELECT: too few arguments" (cadr x)))) `()))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ (_ . pats) (arg . args) env cnds alt)
             x)
           `(select$ ,(cdadr x) ,(cdaddr x) unquote (cdddr x)))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ (() . pats) (arg . args) env (cnd ...) alt)
             x)
           `(select$ ,(cdadr x)
                     ,(cdaddr x)
                     ,(cadddr x)
                     (,@(car (cddddr x)) (null? ,(caaddr x)))
                     unquote
                     (cdr (cddddr x))))
          ((and (syntax-match?
                  '(select$ & => as _ quote quasiquote)
                  '(select$ (pat . pats) (arg . args) env (cnd ...) alt)
                  x)
                (%*literal?*% (caadr x)))
           `(select$ ,(cdadr x)
                     ,(cdaddr x)
                     ,(cadddr x)
                     (,@(car (cddddr x)) (equal? ,(caaddr x) ,(caadr x)))
                     unquote
                     (cdr (cddddr x))))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ (quote . pats) (arg . args) env cnds alt)
             x)
           (let ((%%44 (error "SELECT: 'quote' appears as variable in pattern")))
             `()))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ (quasiquote . pats) (arg . args) env cnds alt)
             x)
           (let ((%%45 (error "SELECT: 'quasiquote' appears as variable in pattern")))
             `()))
          ((and (syntax-match?
                  '(select$ & => as _ quote quasiquote)
                  '(select$ (pat . pats) (arg . args) env cnds alt)
                  x)
                (and (symbol? (caadr x)) (assoc (caadr x) (cadddr x))))
           (let ((%%46 (error "SELECT: duplicate variable in pattern"
                              (caadr x))))
             `()))
          ((and (syntax-match?
                  '(select$ & => as _ quote quasiquote)
                  '(select$ (pat . pats) (arg . args) env cnds alt)
                  x)
                (symbol? (caadr x)))
           `(select$ ,(cdadr x)
                     ,(cdaddr x)
                     ((,(caadr x) unquote (caaddr x)) unquote (cadddr x))
                     unquote
                     (cddddr x)))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ ('e . pats) (arg . args) env (cnd ...) alt)
             x)
           `(select$ ,(cdadr x)
                     ,(cdaddr x)
                     ,(cadddr x)
                     (,@(car (cddddr x)) (equal? ,(caaddr x) ,(caadr x)))
                     unquote
                     (cdr (cddddr x))))
          ((and (syntax-match?
                  '(select$ & => as _ quote quasiquote)
                  '(select$ ((x as pat) . pats) (arg . args) env cnds alt)
                  x)
                (symbol? (caaadr x)))
           `(select$ (,(caaadr x) ,(cadr (cdaadr x)) unquote (cdadr x))
                     (,(caaddr x) unquote (caddr x))
                     unquote
                     (cdddr x)))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ ((x as pat) . pats) (arg . args) env cnds alt)
             x)
           (let ((%%48 (error "SELECT: ill-formed pattern" (caadr x)))) `()))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ ((xp) . pats) (arg . args) env cnds alt)
             x)
           (let ((%%49 (*extend-syntax-add-car* (caaddr x))))
             `(select$ (,(caaadr x) unquote (cdadr x))
                       (,%%49 unquote (cdaddr x))
                       unquote
                       (cdddr x))))
          ((syntax-match?
             '(select$ & => as _ quote quasiquote)
             '(select$ ((xp . yp) . pats) (arg . args) env cnds alt)
             x)
           (let ((%%50 (*extend-syntax-add-car* (caaddr x)))
                 (%%51 (*extend-syntax-add-cdr* (caaddr x))))
             `(select$ (,(caaadr x) ,(cdaadr x) unquote (cdadr x))
                       (,%%50 ,%%51 unquote (cdaddr x))
                       unquote
                       (cdddr x))))
          (else (error "select$: invalid syntax " x)))))

(ux:install-macro
  '*let$*
  (lambda (x)
    (cond ((syntax-match? '(*let$*) '(*let$* bindings . body) x)
           (let ((%%52 (%*del-trivial-bindings*% (cadr x))))
             `(*let$$* ,%%52 unquote (cddr x))))
          (else (error "*let$*: invalid syntax " x)))))

(ux:install-macro
  '*let$$*
  (lambda (x)
    (cond ((syntax-match? '(*let$$*) '(*let$$* () exp) x) (caddr x))
          ((syntax-match? '(*let$$*) '(*let$$* () . body) x)
           `(begin unquote (cddr x)))
          ((syntax-match? '(*let$$*) '(*let$$* bindings . body) x)
           `(let unquote (cdr x)))
          (else (error "*let$$*: invalid syntax " x)))))

