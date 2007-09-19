;;
;; File: X-MACRO.scm
;;


;;; syntax-match? is used by EXTEND-SYNTAX to choose among clauses and
;;; to check for syntactic errors.  It is also available to the user.

(define syntax-match?
  (lambda (keys pat exp)
    (cond
      ((symbol? pat) (if (memq pat keys) (eq? exp pat) #t))
      ((pair? pat)
       (if (equal? (cdr pat) '(...))
           (let f ((lst exp))
             (or (null? lst)
                 (and (pair? lst)
                      (syntax-match? keys (car pat) (car lst))
                      (f (cdr lst)))))
           (and (pair? exp)
                (syntax-match? keys (car pat) (car exp))
                (syntax-match? keys (cdr pat) (cdr exp)))))
      (else (equal? exp pat)))))

;; The procedure UX:INSTALL-MACRO binds a symbol Sym to a procedure Proc,
;; and is called as follows:
;;
;;     (UX:INSTALL-MACRO Sym Proc)
;;
;; Proc should expand forms (Sym ...). For example:
;;
;;     (ux:install-macro 'add3 (lambda (form) `(+ ,(cadr form) 3)))


(define *ux:macro-expanders* '())

(define (ux:install-macro keyword proc)
  (define (lookup-macro sym)
    (let ((association (assq sym *ux:macro-expanders*)))
      (if (not association)
          (begin
            (set! association (cons sym #f))
            (set! *ux:macro-expanders*
                  (cons association *ux:macro-expanders*))))
      association))
  (if (and (symbol? keyword) (procedure? proc))
      (let ((association (lookup-macro keyword)))
        (set-cdr! association proc))
      (error "Ill-formed macro definition" keyword proc)))

;; The procedure (UX:MACRO? Sym) tells whether the symbol Sym
;; is associated with a macro-transformer.

(define (ux:macro? sym)
  (and (assq sym *ux:macro-expanders*) #t))

;; The macro-form (UX:MACRO sym Proc) is equivalent to
;; (UX:INSTALL-MACRO 'Sym Proc).

(ux:install-macro
  'ux:macro
  (lambda (form)
    (if (and (pair? (cdr form))
             (pair? (cddr form))
             (null? (cdddr form)))
        (let ((sym (cadr form))
              (proc (eval (ux:macroexpand (caddr form)))))
          (ux:install-macro sym proc))
        (error "Ill-formed MACRO form"))
    #f))

(define (ux:macroexpand-1 form)
  (cond
    ((not (pair? form)) form)
    ((not (symbol? (car form))) form)
    (else
      (let ((association (assq (car form) *ux:macro-expanders*)))
        (if (and association (cdr association))
            ((cdr association) form)
            form)))))

(define (ux:macroexpand form)

  (define (ill-formed-form form)
    (error "ill-formed form:" form))

  (define (check-syntax pat form)
    (if (not (syntax-match? '() (cdr pat) (cdr form)))
      (ill-formed-form form)))

  (define (macroexpand-quasiquote depth exp)
    (cond
      ((vector? exp)
       (vector-map (lambda (exp0) (macroexpand-quasiquote depth exp0)) exp))
      ((not (pair? exp)) exp)
      ((and (symbol? (car exp)) (pair? (cdr exp)) (null? (cddr exp)))
       (case (car exp)
         ((QUASIQUOTE)
           (list (car exp) (macroexpand-quasiquote (+ depth 1) (cadr exp))))
         ((UNQUOTE UNQUOTE-SPLICING)
           (if (zero? depth)
             (list (car exp) (ux:macroexpand (cadr exp)))
             (list (car exp) (macroexpand-quasiquote (- depth 1) (cadr exp)))))
         (else
           (list (car exp) (macroexpand-quasiquote depth (cadr exp))))))
      (else
        (cons (macroexpand-quasiquote depth (car exp))
              (macroexpand-quasiquote depth (cdr exp))))))

  (define (macroexpand-let key label bindings body)
    (let ((bindings
            (map (lambda (b) `(,(car b) ,(ux:macroexpand (cadr b)))) bindings))
          (body
            (gen-body* (map ux:macroexpand body))))
      (if label
          `(,key ,label ,bindings . ,body)
          `(,key ,bindings . ,body))))

  (define (gen-and forms)
    (cond
      ((null? forms) #t)
      ((null? (cdr forms)) (car forms))
      ((eq? (car forms) #t) (gen-and (cdr forms)))
      ((eq? (car forms) #f) #f)
      (else `(and . ,forms))))

  (define (gen-or forms)
    (cond
      ((null? forms) #f)
      ((null? (cdr forms)) (car forms))
      ((eq? (car forms) #f) (gen-or (cdr forms)))
      ((eq? (car forms) #t) #t)
      (else `(or . ,forms))))

  (define (gen-if3 exp0 exp1 exp2)
    (cond
      ((eq? exp0 #t) exp1)
      ((eq? exp0 #f) exp2)
      (else
        (gen-cond exp0 exp1 exp2))))

  (define (gen-if2 exp0 exp1)
    (cond
      ((eq? exp0 #t) exp1)
      ((eq? exp0 #f) '*UNSPECIFIED*)
      (else
        `(if ,exp0 ,exp1))))

(define (gen-cond exp0 exp1 exp2)
  (cond
    ((and (pair? exp2)
          (eq? (car exp2) 'if)
          (pair? (cdr exp2))
          (pair? (cddr exp2))
          (pair? (cdddr exp2))
          (null? (cddddr exp2)))
     (let ((b (cadddr exp2))
           (a (caddr exp2))
           (p (cadr exp2)))
       `(cond (,exp0 ,exp1) (,p ,a) (else ,b))))
    ((and (pair? exp2) (eq? (car exp2) 'cond))
     (let ((clause* (cdr exp2)))
       `(cond (,exp0 ,exp1) . ,clause*)))
    (else
      `(if ,exp0 ,exp1 ,exp2))))

  (define (gen-body exp)    ;; (let () exp0) ==> exp0
    (if (syntax-match? '(LET) '(LET () body) exp)
      (caddr exp)
      exp))

  (define (gen-body* exps)
    (if (and (pair? exps)
             (null? (cdr exps)))
      (list (gen-body (car exps)))
      exps))

  ;; (write form) (newline)

  (if (not (pair? form))
      form
      (let ((key (car form)) (info (cdr form)))
        (if (not (symbol? key))
            (begin
              (check-syntax '(e e ...) form)
              (map ux:macroexpand form))
            (case key
              ((quote)
               form)
              ((quasiquote)
               (if (and (pair? info) (null? (cdr info)))
                 `(,key ,(macroexpand-quasiquote 0 (car info)))
                 `(,key . ,(map ux:macroexpand info))))
              ((define)
               (check-syntax '(DEFINE v/p e e ...) form)
               `(,key ,(car info)
                 . ,(gen-body* (ux:macroexpand (cdr info)))))
              ((set!)
               (check-syntax '(SET! v e) form)
               `(,key ,(car info) ,(ux:macroexpand (cadr info))))
              ((lambda)
                (check-syntax '(LAMBDA pars e e ...) form)
               `(,key ,(car info)
                 . ,(gen-body* (map ux:macroexpand (cdr info)))))
              ((if)
               (cond
                 ((syntax-match? '() '(IF e e e) form)
                  (gen-if3 (ux:macroexpand (car info))
                           (ux:macroexpand (cadr info))
                           (ux:macroexpand (caddr info))))
                 ((syntax-match? '() '(IF e e) form)
                  (gen-if2 (ux:macroexpand (car info))
                           (ux:macroexpand (cadr info))))
                 (else
                   (ill-formed-form form))))
              ((cond)
               (check-syntax '(COND (p e ...) ...) form)
               `(,key . ,(map ux:macroexpand info)))
              ((let* letrec)
               (check-syntax '(KEYWORD ((v e) ...) e e ...) form)
               (macroexpand-let key #f (car info) (cdr info)))
              ((let)
               (if (and (pair? info)
                        (symbol? (car info)))
                 (begin
                   (check-syntax '(LET l ((v e) ...) e e ...) form)
                   (macroexpand-let key (car info) (cadr info) (cddr info)))
                 (begin
                   (check-syntax '(LET ((v e) ...) e e ...) form)
                   (macroexpand-let key #f (car info) (cdr info)))))
              ((do)
               (check-syntax '(DO ((v e e ...) ...) (p e ...) e ...) form)
               `(,key
                  ,(map (lambda (i) `(,(car i)
                                      . ,(map ux:macroexpand (cdr i))))
                        (car info))
                  . ,(map ux:macroexpand (cdr info))))
              ((case)
               (check-syntax '(CASE e (t e e ...) ...) form)
               `(,key
                  ,(ux:macroexpand (car info))
                  . ,(map (lambda (c) `(,(car c)
                                         . ,(map ux:macroexpand (cdr c))))
                          (cdr info))))
              ((and)
                (check-syntax '(AND e ...) form)
                (gen-and (map ux:macroexpand info)))
              ((or)
                (check-syntax '(OR e ...) form)
                (gen-or (map ux:macroexpand info)))
              (else
                (let ((association (assq (car form) *ux:macro-expanders*)))
                  (if (and association
                           (cdr association))
                      (ux:macroexpand ((cdr association) form))
                      (begin
                        (check-syntax '(e e ...) form)
                        `(,key . ,(map ux:macroexpand info)))))))))))

(define (ux:macroexpand-file ipath opath)
  (call-with-input-file
    ipath
    (lambda (iport)
      (call-with-output-file
        opath
        (lambda (oport)
          (do ((o (read iport) (read iport)))
              ((eof-object? o))
              (pretty-print (ux:macroexpand o) oport)
              (newline oport)
              (display "*")
              ))))))

(define (sex file-name)
  (let ((sex   (string-append file-name ".sex"))
        (scm   (string-append file-name ".scm"))
        )
    (display "Macro expanding: ") (display sex)
    (display " -> ") (display scm) (newline)
    (ux:macroexpand-file sex scm)
    (newline) (display "--- Done ---") (newline)
    ))
