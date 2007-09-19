;;; x-synt.scm
;;; Copyright (C) 1987 R. Kent Dybvig
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.

;;; The basic design of extend-syntax is due to Eugene Kohlbecker.  See
;;; "E. Kohlbecker: Syntactic Extensions in the Programming Language Lisp",
;;; Ph.D.  Dissertation, Indiana University, 1986."  The structure of "with"
;;; pattern/value clauses, the method for compiling extend-syntax into
;;; Scheme code, and the actual implementation are due to Kent Dybvig.

;;; Changed to fit into T Scheme by Lars Ole Andersen (zeus@diku.dk)
;;; Thu May  3 14:56:30 1990

;;; Modified to fit into PC Scheme by Sergei Romanenko.
;;; July 1990, August 1992, January 1993.

;;; Modified to fit into SCM by Sergei Romanneko
;;; October 1993.

(define *extend-syntax-add-car*
  (let ((ca...rs
          '((car . caar) (cdr . cadr)
            (caar . caaar) (cadr . caadr)
            (cdar . cadar) (cddr . caddr)
            (caaar . caaaar) (caadr . caaadr)
            (cadar . caadar) (caddr . caaddr)
            (cdaar . cadaar) (cdadr . cadadr)
            (cddar . caddar) (cdddr . cadddr))))
    (lambda (acc)
      (let ((x (and (pair? acc) (assq (car acc) ca...rs))))
        (if x
            `(,(cdr x) . ,(cdr acc))
            `(car ,acc))))))

(define *extend-syntax-add-cdr*
  (let ((cd...rs
          '((car . cdar) (cdr . cddr)
            (caar . cdaar) (cadr . cdadr)
            (cdar . cddar) (cddr . cdddr)
            (caaar . cdaaar) (caadr . cdaadr)
            (cadar . cdadar) (caddr . cdaddr)
            (cdaar . cddaar) (cdadr . cddadr)
            (cddar . cdddar) (cdddr . cddddr))))
    (lambda (acc)
      (let ((x (and (pair? acc) (assq (car acc) cd...rs))))
        (if x
            `(,(cdr x) . ,(cdr acc))
            `(cdr ,acc))))))

(let ()

    (define (and-map f lst)
      (let loop ((lst lst))
        (or (null? lst)
            (and (f (car lst))
                 (loop (cdr lst))))))

    (define (memq-map x f lst)
      (let loop ((lst lst))
        (and (pair? lst)
             (or (eq? x (f (car lst)))
                 (loop (cdr lst))))))

    (define (id name acc control)
      (list name acc control))

    (define id-name car)
    (define id-access cadr)
    (define id-control caddr)

    (define (loop) (cons '() '()))

    (define loop-ids car)
    (define loop-ids! set-car!)

    (define (duplicate-symbols? lst)
      (and (not (null? lst))
           (or (memq (car lst) (cdr lst))
               (duplicate-symbols? (cdr lst)))))

    (define (unary? exp)
      (and (pair? exp)
           (pair? (cdr exp))
           (null? (cddr exp))))

    (define (quote? exp)
      (and (unary? exp)
           (eq? (car exp) 'QUOTE)))

    (define (quasiquote? exp)
      (and (unary? exp)
           (eq? (car exp) 'QUASIQUOTE)))

    (define (unquote? exp)
      (and (unary? exp)
           (eq? (car exp) 'UNQUOTE)))

    (define (unquote-splicing? exp)
      (and (unary? exp)
           (eq? (car exp) 'UNQUOTE-SPLICING)))

    (define (unquote-/splicing/? exp)
      (and (unary? exp)
           (or (eq? (car exp) 'UNQUOTE)
               (eq? (car exp) 'UNQUOTE-SPLICING))))

    (define (iteration? pat)
      (and (unary? pat)
           (eq? (cadr pat) '...)))

    (define (check-pat keys pat exp)
      (let ((vars
              (let f ((x pat) (vars '()))
                (cond
                  ((iteration? x)
                   (f (car x) vars))
                  ((pair? x)
                   (f (car x) (f (cdr x) vars)))
                  ((symbol? x)
                   (cond
                     ((memq x keys) vars)
                     ((or (eq? x 'with) (eq? x '...))
                      (error "EXTEND-SYNTAX: invalid context for a variable"
                             x exp))
                     (else (cons x vars))))
                  (else vars)))))
        (if (duplicate-symbols? vars)
            (error "EXTEND-SYNTAX: duplicate variable in pattern" pat))
        '()))
    
    (define (parse keys pat acc cntl ids)
      (cond
        ((symbol? pat)
         (if (memq pat keys)
             ids
             (cons (id pat acc cntl) ids)))
        ((iteration? pat)
         (cons
           (id pat acc cntl)
           (let ((x (ux:gentemp)))
             (parse keys (car pat) x (id x acc cntl) ids))))
        ((pair? pat)
         (cons
           (id pat acc cntl)
           (parse keys (car pat) 
                  (*extend-syntax-add-car* acc) cntl
                  (parse keys (cdr pat)
                         (*extend-syntax-add-cdr* acc) cntl
                         ids))))
        (else ids)))

    (define (pattern-variable? sym ids)
      (memq-map sym id-name ids))

    (define (gen keys exp ids loops qqlev)
      (let ((id (lookup exp ids)))
        (cond
          (id
            (add-control! (id-control id) loops exp)
            (list 'UNQUOTE (id-access id)))
          ((not (pair? exp)) exp)
          ((and (quasiquote? exp)
                (not (pattern-variable? 'QUASIQUOTE ids)))
           (list 'UNQUOTE
                 (list 'list
                       ''QUASIQUOTE
                       (make-quasi
                         (gen keys (cadr exp) ids loops
                              (if (= qqlev 0) 0 (+ qqlev 1)))))))
          ((and (unquote-/splicing/? exp)
                (not (pattern-variable? (car exp) ids)))
           (list 'UNQUOTE
                 (if (= qqlev 1)
                     (gen-quotes keys (cadr exp) ids loops)
                     (list 'list
                           (list 'QUOTE (car exp))
                           (make-quasi
                             (gen keys (cadr exp) ids loops (- qqlev 1)))))))
          ((and (eq? (car exp) 'with)
                (not (pattern-variable? 'with ids)))
           (if (not (syntax-match? '(with) '(with ((p x) ...) e) exp))
               (error "EXTEND-SYNTAX: invalid 'with' form" exp))
           (check-pat keys (map car (cadr exp)) exp)
           (list 'UNQUOTE
                 (gen-with
                   keys
                   (map car (cadr exp)) (map cadr (cadr exp))
                   (caddr exp) ids loops)))
          ((and (pair? (cdr exp)) (eq? (cadr exp) '...))
           (let ((x (loop)))
             (gen-cons (list 'UNQUOTE-SPLICING
                             (make-loop x
                                        (gen keys (car exp) ids
                                             (cons x loops) qqlev)
                                        exp))
                       (gen keys (cddr exp) ids loops qqlev))))
          (else
            (gen-cons (gen keys (car exp) ids loops qqlev)
                      (gen keys (cdr exp) ids loops qqlev))))))

    (define (gen-cons head tail)
      (if (null? tail)
          (if (unquote-splicing? head)
              (list 'UNQUOTE (cadr head))
              (cons head tail))
          (cons head tail)))

    (define (gen-with keys pats exps body ids loops)
      (let ((vars (map (lambda (x) (ux:gentemp)) pats)))
        `(let ,(map
                 (lambda (v e) `(,v ,(gen-quotes keys e ids loops)))
                 vars exps)
           .
           ,(let f ((ps pats) (vs vars))
              (if (null? ps)
                  (let f ((pats pats) (vars vars) (ids ids))
                    (if (null? pats)
                        `(,(make-quasi (gen keys body ids loops 0)))
                        (f (cdr pats)
                           (cdr vars)
                           (parse '() (car pats) (car vars) '() ids))))
                  (let ((m (car ps)))
                    (if (symbol? m)
                        (f (cdr ps) (cdr vs))
                        `((if
                            (not (syntax-match? '() ',m ,(car vs)))
                            (error
                              ,(string-append
                                 (symbol->string (car keys))
                                 ": 'with' pattern does not fit the argument")
                              ',(car ps) ,(car vs)))
                          . ,(f (cdr ps) (cdr vs))))))))))

    (define (gen-quotes keys exp ids loops)
      (cond
        ((quote? exp)
         (make-quasi (gen keys (cadr exp) ids loops 0)))
        ((quasiquote? exp)
         (make-quasi (gen keys (cadr exp) ids loops 1)))
        (else
          (let f ((exp exp))
            (if (pair? exp)
                (cons (gen-quotes keys (car exp) ids loops)
                      (f (cdr exp)))
                exp)))))

    (define (lookup exp ids)
      (let loop ((ls ids))
        (cond
          ((null? ls) #f)
          ((equal? (id-name (car ls)) exp) (car ls))
          ((subexp? (id-name (car ls)) exp) #f)
          (else (loop (cdr ls))))))

    (define (subexp? exp1 exp2)
      (and (symbol? exp1)
           (let f ((exp2 exp2))
             (or (eq? exp1 exp2)
                 (and (pair? exp2)
                      (or (f (car exp2))
                          (f (cdr exp2))))))))

    (define (add-control! id loops exp)
      (if (not (null? id))
          (begin
            (if (null? loops)
                (error "EXTEND-SYNTAX: missing ellipsis in expansion" exp))
            (let ((x (loop-ids (car loops))))
              (if (not (memq id x))
                  (loop-ids! (car loops) (cons id x))))
            (add-control! (id-control id) (cdr loops) exp))))

    (define (make-loop loop body exp)
      (let ((ids (loop-ids loop)))
        (if (null? ids)
            (error "EXTEND-SYNTAX: extra ellipsis in expansion" exp))
        (cond
          ((and (null? (cdr ids))
                (unquote? body)
                (eq? (cadr body) (id-name (car ids))))
           (id-access (car ids)))
          ((and (null? (cdr ids))
                (unquote? body)
                (unary? (cadr body))
                (eq? (cadadr body) (id-name (car ids))))
           `(map ,(caadr body) ,(id-access (car ids))))
          (else
            `(map
               (lambda ,(map id-name ids) ,(make-quasi body))
                       . ,(map id-access ids))))))

    (define (make-quasi exp)
      (if (and (pair? exp) (eq? (car exp) 'UNQUOTE))
          (cadr exp)
          (list 'QUASIQUOTE exp)))

    (define (make-clause keys cl x)
      (cond
        ((syntax-match? '() '(pat fender exp) cl)
         (let ((pat (car cl)) (fender (cadr cl)) (exp (caddr cl)))
           (check-pat keys pat pat)
           (let ((ids (parse keys pat x '() '())))
             `((and (syntax-match? ',keys ',pat ,x)
                    ,(gen-quotes keys fender ids '()))
               ,(make-quasi (gen keys exp ids '() 0))))))
        ((syntax-match? '() '(pat exp) cl)
         (let ((pat (car cl)) (exp (cadr cl)))
           (check-pat keys pat pat)
           (let ((ids (parse keys pat x '() '())))
             `((syntax-match? ',keys ',pat ,x)
               ,(make-quasi (gen keys exp ids '() 0))))))
        (else
          (error "EXTEND-SYNTAX: invalid clause " cl))))

    (define make-syntax
      (let ((x 'x))
        (lambda (keys clauses)
          (if (memq '... keys)
              (error "EXTEND-SYNTAX: invalid keyword ... in keyword list "
                     keys))
          `(lambda (,x)
             (cond
               ,@(map (lambda (cl) (make-clause keys cl x)) clauses)
               (else
                 (error ,(string-append (symbol->string (car keys))
                                        ": invalid syntax ")
                        ,x)))))))

    ;;:: Main function ;;;;

  (define (extend-syntax e)
    (if (not (syntax-match? '(EXTEND-SYNTAX)
                            '(EXTEND-SYNTAX (key key ...) clause ...)
                            e))
        (error "EXTEND-SYNTAX: invalid syntax" e))
    (let ((keys (cadr e)) (clauses (cddr e)))
      (if (not (and-map symbol? keys))
          (error "EXTEND-SYNTAX: invalid keyword in keyword list" e))
      `(ux:install-macro ',(car keys) ,(make-syntax keys clauses))
      ))

  (ux:install-macro 'extend-syntax extend-syntax)

  )
