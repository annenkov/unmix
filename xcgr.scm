(define (ucgr:main src dst src-prog)
  (define (find-cutpoints prog)
    (define visited-funcs #f)
    (define cutpoints #f)
    (define (visit! fcn path)
      (let ((%%91 (assq fcn prog)))
        (let ((body (cadddr %%91)))
          (if (not (memq fcn visited-funcs))
            (begin (set! visited-funcs `(,fcn unquote visited-funcs))))
          (find-cycles! body `(,fcn unquote path)))))
    (define (find-cycles! exp path)
      (cond ((symbol? exp) #f)
            ((equal? (car exp) 'quote) #f)
            ((equal? (car exp) 'call)
             (let ((exp* (cddr exp)) (fname (cadr exp)))
               (cond ((memq fname path)
                      (if (not (memq fname cutpoints))
                        (begin (set! cutpoints `(,fname unquote cutpoints))))
                      (find-cycles*! exp* path))
                     ((memq fname visited-funcs) (find-cycles*! exp* path))
                     (else (visit! fname path) (find-cycles*! exp* path)))))
            ((equal? (car exp) 'xcall)
             (let ((exp* (cddr exp)) (fname (cadr exp)))
               (find-cycles*! exp* path)))
            (else
             (let ((exp* (cdr exp)) (op (car exp)))
               (find-cycles*! exp* path)))))
    (define (find-cycles*! exp* path)
      (for-each (lambda (exp) (find-cycles! exp path)) exp*))
    (set! visited-funcs '())
    (set! cutpoints '())
    (let ((goal (caar prog))) (visit! goal '()) cutpoints))
  (define (unfold suspend prog)
    (define pending #f)
    (define current #f)
    (define out #f)
    (define (choose-next!)
      (if (not (null? pending))
        (begin
          (set! current (car pending))
          (set! pending (cdr pending))
          (set! out `(,(make-fundef!) unquote out))
          (choose-next!))))
    (define (make-fundef!)
      (let ((%%92 (assq current prog)))
        (let ((body (cadddr %%92)) (parlist (cadr %%92)))
          `(,current ,parlist = ,(reduce-exp! body parlist parlist)))))
    (define (reduce-exp! exp vn vv)
      (cond ((let ((vname exp)) (symbol? vname))
             (let ((vname exp)) (lookup-value vname vn vv)))
            ((equal? (car exp) 'quote) (let ((const (cadr exp))) exp))
            ((equal? (car exp) 'car)
             (let ((exp1 (cadr exp))) (reduce-car (reduce-exp! exp1 vn vv))))
            ((equal? (car exp) 'cdr)
             (let ((exp1 (cadr exp))) (reduce-cdr (reduce-exp! exp1 vn vv))))
            ((equal? (car exp) 'pair?)
             (let ((exp1 (cadr exp))) (reduce-pair? (reduce-exp! exp1 vn vv))))
            ((equal? (car exp) 'null?)
             (let ((exp1 (cadr exp)))
               (reduce-1-op 'null? null? (reduce-exp! exp1 vn vv))))
            ((equal? (car exp) 'not)
             (let ((exp1 (cadr exp)))
               (reduce-1-op 'not not (reduce-exp! exp1 vn vv))))
            ((equal? (car exp) 'cons)
             (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
               (reduce-cons
                 (reduce-exp! exp1 vn vv)
                 (reduce-exp! exp2 vn vv))))
            ((equal? (car exp) 'equal?)
             (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
               (reduce-2-op
                 'equal?
                 equal?
                 (reduce-exp! exp1 vn vv)
                 (reduce-exp! exp2 vn vv))))
            ((equal? (car exp) 'eq?)
             (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
               (reduce-2-op
                 'eq?
                 eq?
                 (reduce-exp! exp1 vn vv)
                 (reduce-exp! exp2 vn vv))))
            ((equal? (car exp) 'eqv?)
             (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
               (reduce-2-op
                 'eqv?
                 eqv?
                 (reduce-exp! exp1 vn vv)
                 (reduce-exp! exp2 vn vv))))
            ((equal? (car exp) 'if)
             (let ((exp3 (cadddr exp)) (exp2 (caddr exp)) (exp1 (cadr exp)))
               (reduce-if! (reduce-exp! exp1 vn vv) exp2 exp3 vn vv)))
            ((equal? (car exp) 'call)
             (let ((exp* (cddr exp)) (fname (cadr exp)))
               (let ((arg* (reduce-exp*! exp* vn vv)))
                 (if (memq fname suspend)
                   (reduce-call! fname arg*)
                   (let ((%%93 (assq fname prog)))
                     (let ((body (cadddr %%93)) (parlist (cadr %%93)))
                       (if (suspend? parlist arg* body)
                         (reduce-call! fname arg*)
                         (reduce-exp! body parlist arg*))))))))
            ((equal? (car exp) 'xcall)
             (let ((exp* (cddr exp)) (fname (cadr exp)))
               `(xcall ,fname unquote (reduce-exp*! exp* vn vv))))
            ((equal? (car exp) 'xapply)
             (let ((exp* (cdr exp)))
               (reduce-xapply (reduce-exp*! exp* vn vv))))
            (else
             (let ((exp* (cdr exp)) (fname (car exp)))
               `(,fname unquote (reduce-exp*! exp* vn vv))))))
    (define (reduce-exp*! exp* vn vv)
      (map (lambda (exp) (reduce-exp! exp vn vv)) exp*))
    (define (reduce-call! fname exp*)
      (if (and (not (assq fname out))
               (not (eq? fname current))
               (not (memq fname pending)))
        (begin (set! pending `(,fname unquote pending))))
      `(call ,fname unquote exp*))
    (define (reduce-if! cnd exp1 exp2 vn vv)
      (cond ((and (pair? cnd)
                  (equal? (car cnd) 'quote)
                  (pair? (cdr cnd))
                  (null? (cddr cnd)))
             (let ((const (cadr cnd)))
               (if const (reduce-exp! exp1 vn vv) (reduce-exp! exp2 vn vv))))
            ((and (pair? cnd) (equal? (car cnd) 'cons))
             (reduce-exp! exp1 vn vv))
            (else
             `(if ,cnd ,(reduce-exp! exp1 vn vv) ,(reduce-exp! exp2 vn vv)))))
    (define (reduce-xapply args)
      (if (and (pair? args)
               (pair? (car args))
               (equal? (caar args) 'quote)
               (pair? (cdar args))
               (null? (cddar args))
               (pair? (cdr args))
               (null? (cddr args))
               (let ((exp (cadr args)) (fname (cadar args)))
                 (and (symbol? fname) (splittable-exp? exp))))
        (let ((exp (cadr args)) (fname (cadar args)))
          (make-xcall fname (split-exp exp)))
        `(xapply unquote args)))
    (define (splittable-exp? exp)
      (cond ((equal? (car exp) 'quote) (let ((c (cadr exp))) (list? c)))
            ((equal? (car exp) 'cons)
             (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
               (splittable-exp? exp2)))
            (else (error "SELECT: no match for" exp))))
    (define (split-exp exp)
      (cond ((equal? (car exp) 'quote) (let ((c (cadr exp))) (split-quote c)))
            ((equal? (car exp) 'cons)
             (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
               `(,exp1 unquote (split-exp exp2))))
            (else (error "SELECT: no match for" exp))))
    (define (split-quote lst)
      (if (null? lst)
        '()
        (let ((c2 (cdr lst)) (c1 (car lst)))
          `(',c1 unquote (split-quote c2)))))
    (define (make-xcall fname exp*)
      (if (memq fname '(static ifs ifd call xcall))
        `(xcall ,fname unquote exp*)
        `(,fname unquote exp*)))
    (define (reduce-car exp)
      (cond ((and (pair? exp)
                  (equal? (car exp) 'quote)
                  (pair? (cdr exp))
                  (pair? (cadr exp))
                  (null? (cddr exp)))
             (let ((c2 (cdadr exp)) (c1 (caadr exp))) `',c1))
            ((and (pair? exp)
                  (equal? (car exp) 'cons)
                  (pair? (cdr exp))
                  (pair? (cddr exp))
                  (null? (cdddr exp)))
             (let ((e2 (caddr exp)) (e1 (cadr exp))) e1))
            (else `(car ,exp))))
    (define (reduce-cdr exp)
      (cond ((and (pair? exp)
                  (equal? (car exp) 'quote)
                  (pair? (cdr exp))
                  (pair? (cadr exp))
                  (null? (cddr exp)))
             (let ((c2 (cdadr exp)) (c1 (caadr exp))) `',c2))
            ((and (pair? exp)
                  (equal? (car exp) 'cons)
                  (pair? (cdr exp))
                  (pair? (cddr exp))
                  (null? (cdddr exp)))
             (let ((e2 (caddr exp)) (e1 (cadr exp))) e2))
            (else `(cdr ,exp))))
    (define (reduce-pair? exp)
      (cond ((and (pair? exp)
                  (equal? (car exp) 'quote)
                  (pair? (cdr exp))
                  (null? (cddr exp)))
             (let ((c1 (cadr exp))) `',(pair? c1)))
            ((and (pair? exp)
                  (equal? (car exp) 'cons)
                  (pair? (cdr exp))
                  (pair? (cddr exp))
                  (null? (cdddr exp)))
             (let ((e2 (caddr exp)) (e1 (cadr exp))) ''#t))
            (else `(pair? ,exp))))
    (define (reduce-cons exp1 exp2)
      (if (and (pair? exp1)
               (equal? (car exp1) 'quote)
               (pair? (cdr exp1))
               (null? (cddr exp1))
               (pair? exp2)
               (equal? (car exp2) 'quote)
               (pair? (cdr exp2))
               (null? (cddr exp2)))
        (let ((c2 (cadr exp2)) (c1 (cadr exp1))) `',(cons c1 c2))
        `(cons ,exp1 ,exp2)))
    (define (reduce-1-op name proc exp)
      (if (and (pair? exp)
               (equal? (car exp) 'quote)
               (pair? (cdr exp))
               (null? (cddr exp)))
        (let ((c (cadr exp))) `',(proc c))
        `(,name ,exp)))
    (define (reduce-2-op name proc exp1 exp2)
      (if (and (pair? exp1)
               (equal? (car exp1) 'quote)
               (pair? (cdr exp1))
               (null? (cddr exp1))
               (pair? exp2)
               (equal? (car exp2) 'quote)
               (pair? (cdr exp2))
               (null? (cddr exp2)))
        (let ((c2 (cadr exp2)) (c1 (cadr exp1))) `',(proc c1 c2))
        `(,name ,exp1 ,exp2)))
    (define (suspend? vn vv exp)
      (if (and (null? vn) (null? vv))
        #f
        (let ((vvtl (cdr vv)) (vvhd (car vv)) (vntl (cdr vn)) (vnhd (car vn)))
          (or (and (too-large? vvhd) (> (max-occurrences vnhd exp) 1))
              (suspend? vntl vvtl exp)))))
    (define (too-large? exp)
      (cond ((symbol? exp) #f)
            ((equal? (car exp) 'quote)
             (let ((c (cadr exp))) (too-large-constant? c)))
            (else #t)))
    (define (too-large-constant? c) (if (pair? c) #t #f))
    (define (max-occurrences vname exp)
      (cond ((symbol? exp) (if (eq? vname exp) 1 0))
            ((equal? (car exp) 'quote) 0)
            ((equal? (car exp) 'if)
             (let ((exp3 (cadddr exp)) (exp2 (caddr exp)) (exp1 (cadr exp)))
               (let ((n1 (max-occurrences vname exp1))
                     (n2 (max-occurrences vname exp2))
                     (n3 (max-occurrences vname exp3)))
                 (max (+ n1 n2) (+ n1 n3)))))
            ((equal? (car exp) 'call)
             (let ((exp* (cddr exp))) (max-occurrences* vname exp*)))
            ((equal? (car exp) 'xcall)
             (let ((exp* (cddr exp))) (max-occurrences* vname exp*)))
            (else (let ((exp* (cdr exp))) (max-occurrences* vname exp*)))))
    (define (max-occurrences* vname exp*)
      (foldl-map + 0 (lambda (exp) (max-occurrences vname exp)) exp*))
    (define (lookup-value vname vn vv)
      (let ((vvtl (cdr vv)) (vvhd (car vv)) (vntl (cdr vn)) (vnhd (car vn)))
        (if (eq? vnhd vname) vvhd (lookup-value vname vntl vvtl))))
    (let ((goal (caar prog)))
      (set! pending `(,goal))
      (set! out '())
      (choose-next!)
      (reverse out)))
  (newline)
  (display "-- Call Graph Reduction:  ")
  (display src)
  (display " -> ")
  (display dst)
  (newline)
  (newline)
  (display "Call Graph Analysis")
  (newline)
  (let ((dontunfold (find-cutpoints src-prog)))
    (display "Cut Points: ")
    (newline)
    (write dontunfold)
    (newline)
    (display "Call Unfolding")
    (newline)
    (let ((dst-prog (unfold dontunfold src-prog)))
      (display "-- Done --")
      (newline)
      dst-prog)))

