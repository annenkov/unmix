(define ($specialize-fundef-$1 conf)
  (if (null? conf)
    '$specialize-fundef
    ($check-function-$1 (car conf) (cdr conf))))

(define ($check-function-$1 fname svv)
  (cond ((eq? fname '$specialize-fundef) `((conf) ,($pe-exp-$1 (car svv))))
        ((eq? fname '$check-function)
         `((fname svv) ,($pe-exp-$2 (car svv) (cadr svv) (caddr svv))))
        ((eq? fname '$pe-exp)
         `((svv dvv)
           ,($pe-exp-$3
              (car svv)
              (cadr svv)
              (caddr svv)
              (cadddr svv)
              (car (cddddr svv)))))
        ((eq? fname '$eval-exp)
         `((svv) ,($pe-exp-$4 (car svv) (cadr svv) (caddr svv))))
        (else '())))

(define ($pe-exp-$4 svv-$1 svv-$2 svv-$3)
  (cond ((symbol? svv-$1) ($pe-exp-$5 svv-$1 svv-$2 'svv))
        ((equal? (car svv-$1) 'quote) `',(cadr svv-$1))
        ((equal? (car svv-$1) 'if)
         `(if ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)
            ,($pe-exp-$4 (caddr svv-$1) svv-$2 svv-$3)
            ,($pe-exp-$4 (cadddr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'call)
         ($pe-exp-$7
           svv-$3
           (assq (cadr svv-$1) svv-$3)
           ($pe-exp-$6 (cddr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'xcall)
         `(xapply ',(cadr svv-$1) ,($pe-exp-$6 (cddr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'error)
         `(error '"Error function encountered during partial evaluation"
                 (cons 'error ,($pe-exp-$6 (cdr svv-$1) svv-$2 svv-$3))))
        ((equal? (car svv-$1) 'car)
         `(car ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'cdr)
         `(cdr ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'cons)
         `(cons ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)
                ,($pe-exp-$4 (caddr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'null?)
         `(null? ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'pair?)
         `(pair? ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'equal?)
         `(equal? ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)
                  ,($pe-exp-$4 (caddr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'eq?)
         `(eq? ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)
               ,($pe-exp-$4 (caddr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'eqv?)
         `(eqv? ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)
                ,($pe-exp-$4 (caddr svv-$1) svv-$2 svv-$3)))
        ((equal? (car svv-$1) 'not)
         `(not ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$3)))
        (else
         `(xapply ',(car svv-$1) ,($pe-exp-$6 (cdr svv-$1) svv-$2 svv-$3)))))

(define ($pe-exp-$7 svv-$1 svv-$2 dvv-$1)
  `(call ($eval-exp ,(cadddr svv-$2) ,(cadr svv-$2) ,svv-$1) ,dvv-$1))

(define ($pe-exp-$6 svv-$1 svv-$2 svv-$3)
  (if (null? svv-$1)
    ''()
    `(cons ,($pe-exp-$4 (car svv-$1) svv-$2 svv-$3)
           ,($pe-exp-$6 (cdr svv-$1) svv-$2 svv-$3))))

(define ($pe-exp-$5 svv-$1 svv-$2 dvv-$1)
  (if (eq? (car svv-$2) svv-$1)
    `(car ,dvv-$1)
    ($pe-exp-$5 svv-$1 (cdr svv-$2) `(cdr ,dvv-$1))))

(define ($pe-exp-$3 svv-$1 svv-$2 svv-$3 svv-$4 svv-$5)
  (cond ((symbol? svv-$1) ($pe-exp-$5 svv-$1 svv-$3 'dvv))
        ((equal? (car svv-$1) 'static)
         `(cons 'quote (cons ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$4) '())))
        ((equal? (car svv-$1) 'ifs)
         `(if ,($pe-exp-$4 (cadr svv-$1) svv-$2 svv-$4)
            ,($pe-exp-$3 (caddr svv-$1) svv-$2 svv-$3 svv-$4 svv-$5)
            ,($pe-exp-$3 (cadddr svv-$1) svv-$2 svv-$3 svv-$4 svv-$5)))
        ((equal? (car svv-$1) 'ifd)
         `(cons 'if
                (cons ,($pe-exp-$3 (cadr svv-$1) svv-$2 svv-$3 svv-$4 svv-$5)
                      (cons ,($pe-exp-$3
                               (caddr svv-$1)
                               svv-$2
                               svv-$3
                               svv-$4
                               svv-$5)
                            (cons ,($pe-exp-$3
                                     (cadddr svv-$1)
                                     svv-$2
                                     svv-$3
                                     svv-$4
                                     svv-$5)
                                  '())))))
        ((equal? (car svv-$1) 'call)
         ($pe-exp-$9
           (assq (cadr svv-$1) svv-$5)
           svv-$4
           svv-$5
           ($pe-exp-$6 (caddr svv-$1) svv-$2 svv-$4)
           ($pe-exp-$8 (cadddr svv-$1) svv-$2 svv-$3 svv-$4 svv-$5)))
        ((equal? (car svv-$1) 'rcall)
         `(cons 'call
                (cons (cons ',(cadr svv-$1)
                            ,($pe-exp-$6 (caddr svv-$1) svv-$2 svv-$4))
                      ,($pe-exp-$8
                         (cadddr svv-$1)
                         svv-$2
                         svv-$3
                         svv-$4
                         svv-$5))))
        ((equal? (car svv-$1) 'xcall)
         `(cons 'xcall
                (cons ',(cadr svv-$1)
                      ,($pe-exp-$8
                         (cddr svv-$1)
                         svv-$2
                         svv-$3
                         svv-$4
                         svv-$5))))
        (else
         `(cons ',(car svv-$1)
                ,($pe-exp-$8 (cdr svv-$1) svv-$2 svv-$3 svv-$4 svv-$5)))))

(define ($pe-exp-$9 svv-$1 svv-$2 svv-$3 dvv-$1 dvv-$2)
  `(call ($pe-exp ,(car (cddddr svv-$1))
                  ,(cadr svv-$1)
                  ,(caddr svv-$1)
                  ,svv-$2
                  ,svv-$3)
         ,dvv-$1
         ,dvv-$2))

(define ($pe-exp-$8 svv-$1 svv-$2 svv-$3 svv-$4 svv-$5)
  (if (null? svv-$1)
    ''()
    `(cons ,($pe-exp-$3 (car svv-$1) svv-$2 svv-$3 svv-$4 svv-$5)
           ,($pe-exp-$8 (cdr svv-$1) svv-$2 svv-$3 svv-$4 svv-$5))))

(define ($pe-exp-$2 svv-$1 svv-$2 svv-$3)
  (if (null? svv-$1) ''() ($pe-exp-$10 (car svv-$1) svv-$3 svv-$2 svv-$1)))

(define ($pe-exp-$10 svv-$1 svv-$2 svv-$3 svv-$4)
  `(if (eq? fname ',svv-$1)
     ,($pe-exp-$11 (assq svv-$1 svv-$2) svv-$3 svv-$2)
     ,($pe-exp-$2 (cdr svv-$4) svv-$3 svv-$2)))

(define ($pe-exp-$11 svv-$1 svv-$2 svv-$3)
  ($pe-exp-$12 (caddr svv-$1) (cadr svv-$1) svv-$1 svv-$2 svv-$3))

(define ($pe-exp-$12 svv-$1 svv-$2 svv-$3 svv-$4 svv-$5)
  `(cons ',svv-$1
         (cons (call ($pe-exp ,(car (cddddr svv-$3))
                              ,svv-$2
                              ,svv-$1
                              ,svv-$4
                              ,svv-$5)
                     ,($pe-exp-$13 svv-$2 'svv)
                     ',svv-$1)
               '())))

(define ($pe-exp-$13 svv-$1 dvv-$1)
  (if (null? svv-$1)
    ''()
    `(cons (car ,dvv-$1) ,($pe-exp-$13 (cdr svv-$1) `(cdr ,dvv-$1)))))

(define ($pe-exp-$1 svv-$1)
  `(if (null? conf)
     ',(caar svv-$1)
     (call ($check-function ,(car svv-$1) ,(caddr svv-$1) ,(cadr svv-$1))
           (car conf)
           (cdr conf))))

