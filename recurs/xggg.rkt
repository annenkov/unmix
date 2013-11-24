(define ($start sv)
  `(,(umainpe:print-fundef!
       '$start
       '(sv)
       `(call ,(cdr (umainpe:find-name! '$specialize-fundef sv)) sv))
    ,(umainpe:print-fundef!
       (cdr (umainpe:find-name! '$specialize-fundef sv))
       '(sv)
       ($pe-exp-$1 (car sv)))))

(define ($pe-exp-$1 sv-$1) ($pe-exp-$2 (cadr sv-$1) (caar sv-$1) sv-$1))

(define ($pe-exp-$2 sv-$1 sv-$2 sv-$3)
  ($pe-exp-$3 (assq sv-$2 sv-$1) sv-$2 sv-$3 sv-$1))

(define ($pe-exp-$3 sv-$1 sv-$2 sv-$3 sv-$4)
  ($pe-exp-$5
    (caddr sv-$1)
    sv-$1
    (cadr sv-$1)
    sv-$3
    sv-$4
    `(cdr (umainpe:find-name! ',sv-$2 sv))))

(define ($pe-exp-$5 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5 dv-$1)
  `(cons (umainpe:print-fundef!
           '$start
           ',sv-$1
           (cons 'call (cons ,dv-$1 ',sv-$1)))
         (cons (umainpe:print-fundef!
                 ,dv-$1
                 ',sv-$1
                 (call ,($gen-res-fundef!-$1
                          (car (cddddr sv-$2))
                          sv-$3
                          sv-$1
                          (caddr sv-$4)
                          sv-$5)
                       ,($pe-exp-$17 sv-$3 'sv)
                       ',sv-$1))
               '())))

(define ($pe-exp-$17 sv-$1 dv-$1)
  (if (null? sv-$1)
    ''()
    `(cons (car ,dv-$1) ,($pe-exp-$17 (cdr sv-$1) `(cdr ,dv-$1)))))

(define ($gen-res-fundef!-$1 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5)
  ($gen-res-fundef-p!-$1
    (umainpe:find-name! '$pe-exp `(,sv-$1 ,sv-$2 ,sv-$3 ,sv-$4 ,sv-$5))
    `(,sv-$1 ,sv-$2 ,sv-$3 ,sv-$4 ,sv-$5)))

(define ($gen-res-fundef-p!-$1 fn sv)
  (if (car fn)
    (umainpe:print-fundef!
      (cdr fn)
      '(sv dv)
      ($pe-exp-$6 (car sv) (cadr sv) (caddr sv) (cadddr sv) (car (cddddr sv))))
    (cdr fn)))

(define ($pe-exp-$6 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5)
  (cond ((not (pair? sv-$1)) ($pe-exp-$7 sv-$1 sv-$3 'dv))
        ((equal? (car sv-$1) 'static)
         `(cons 'quote (cons ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$4) '())))
        ((equal? (car sv-$1) 'ifs)
         `(if ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$4)
            ,($pe-exp-$6 (caddr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5)
            ,($pe-exp-$6 (cadddr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5)))
        ((equal? (car sv-$1) 'ifd)
         `(cons 'if
                (cons ,($pe-exp-$6 (cadr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5)
                      (cons ,($pe-exp-$6 (caddr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5)
                            (cons ,($pe-exp-$6
                                     (cadddr sv-$1)
                                     sv-$2
                                     sv-$3
                                     sv-$4
                                     sv-$5)
                                  '())))))
        ((equal? (car sv-$1) 'call)
         ($pe-exp-$12
           (assq (cadr sv-$1) sv-$5)
           sv-$4
           sv-$5
           ($pe-exp-$9 (caddr sv-$1) sv-$2 sv-$4)
           ($pe-exp-$11 (cadddr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5)))
        ((equal? (car sv-$1) 'rcall)
         `(cons 'call
                (cons (call ,($gen-res-fundef!-$3 (cadr sv-$1) sv-$4 sv-$5)
                            ,($pe-exp-$9 (caddr sv-$1) sv-$2 sv-$4))
                      ,($pe-exp-$11 (cadddr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5))))
        ((equal? (car sv-$1) 'xcall)
         `(cons 'xcall
                (cons ',(cadr sv-$1)
                      ,($pe-exp-$11 (cddr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5))))
        (else
         `(cons ',(car sv-$1)
                ,($pe-exp-$11 (cdr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5)))))

(define ($gen-res-fundef!-$3 sv-$1 sv-$2 sv-$3)
  ($gen-res-fundef-p!-$3
    (umainpe:find-name! '$gen-res-fundef! `(,sv-$1 ,sv-$2 ,sv-$3))
    sv-$1
    sv-$2
    sv-$3))

(define ($gen-res-fundef-p!-$3 fn sv-$1 sv-$2 sv-$3)
  (if (car fn)
    (umainpe:print-fundef!
      (cdr fn)
      '(sv)
      ($pe-exp-$14 (assq sv-$1 sv-$3) sv-$1 sv-$2 sv-$3))
    (cdr fn)))

(define ($pe-exp-$14 sv-$1 sv-$2 sv-$3 sv-$4)
  ($pe-exp-$15 (cadr sv-$1) sv-$2 sv-$1 sv-$3 sv-$4))

(define ($pe-exp-$15 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5)
  `(call ,($gen-res-fundef!-$4
            (car (cddddr sv-$3))
            sv-$1
            (caddr sv-$3)
            sv-$4
            sv-$5)
         (umainpe:find-name! ',sv-$2 ,($pe-exp-$17 sv-$1 'sv))
         sv))

(define ($gen-res-fundef!-$4 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5)
  ($gen-res-fundef-p!-$4
    (umainpe:find-name!
      '$gen-res-fundef-p!
      `(,sv-$1 ,sv-$2 ,sv-$3 ,sv-$4 ,sv-$5))
    `(,sv-$1 ,sv-$2 ,sv-$3 ,sv-$4 ,sv-$5)))

(define ($gen-res-fundef-p!-$4 fn sv)
  (if (car fn)
    (umainpe:print-fundef!
      (cdr fn)
      '(fn sv)
      ($pe-exp-$16
        (car sv)
        (cadr sv)
        (caddr sv)
        (cadddr sv)
        (car (cddddr sv))))
    (cdr fn)))

(define ($pe-exp-$16 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5)
  `(if (car fn)
     (umainpe:print-fundef!
       (cdr fn)
       ',sv-$3
       (call ,($gen-res-fundef!-$1 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5) sv ',sv-$3))
     (cdr fn)))

(define ($pe-exp-$12 sv-$1 sv-$2 sv-$3 dv-$1 dv-$2)
  `(call ,($gen-res-fundef!-$1
            (car (cddddr sv-$1))
            (cadr sv-$1)
            (caddr sv-$1)
            sv-$2
            sv-$3)
         ,dv-$1
         ,dv-$2))

(define ($pe-exp-$11 sv-$1 sv-$2 sv-$3 sv-$4 sv-$5)
  (if (null? sv-$1)
    ''()
    `(cons ,($pe-exp-$6 (car sv-$1) sv-$2 sv-$3 sv-$4 sv-$5)
           ,($pe-exp-$11 (cdr sv-$1) sv-$2 sv-$3 sv-$4 sv-$5))))

(define ($pe-exp-$9 sv-$1 sv-$2 sv-$3)
  (if (null? sv-$1)
    ''()
    `(cons ,($pe-exp-$8 (car sv-$1) sv-$2 sv-$3)
           ,($pe-exp-$9 (cdr sv-$1) sv-$2 sv-$3))))

(define ($pe-exp-$8 sv-$1 sv-$2 sv-$3)
  (cond ((not (pair? sv-$1)) ($pe-exp-$7 sv-$1 sv-$2 'sv))
        ((equal? (car sv-$1) 'quote) `',(cadr sv-$1))
        ((equal? (car sv-$1) 'if)
         `(if ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)
            ,($pe-exp-$8 (caddr sv-$1) sv-$2 sv-$3)
            ,($pe-exp-$8 (cadddr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'call)
         ($pe-exp-$10
           sv-$3
           (assq (cadr sv-$1) sv-$3)
           ($pe-exp-$9 (cddr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'xcall)
         `(xapply ',(cadr sv-$1) ,($pe-exp-$9 (cddr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'error)
         `(error '"Error function encountered during partial evaluation"
                 (cons 'error ,($pe-exp-$9 (cdr sv-$1) sv-$2 sv-$3))))
        ((equal? (car sv-$1) 'car)
         `(car ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'cdr)
         `(cdr ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'cons)
         `(cons ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)
                ,($pe-exp-$8 (caddr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'null?)
         `(null? ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'pair?)
         `(pair? ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'equal?)
         `(equal? ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)
                  ,($pe-exp-$8 (caddr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'eq?)
         `(eq? ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)
               ,($pe-exp-$8 (caddr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'eqv?)
         `(eqv? ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)
                ,($pe-exp-$8 (caddr sv-$1) sv-$2 sv-$3)))
        ((equal? (car sv-$1) 'not)
         `(not ,($pe-exp-$8 (cadr sv-$1) sv-$2 sv-$3)))
        (else `(xapply ',(car sv-$1) ,($pe-exp-$9 (cdr sv-$1) sv-$2 sv-$3)))))

(define ($pe-exp-$10 sv-$1 sv-$2 dv-$1)
  `(call ,($gen-res-fundef!-$2 (cadddr sv-$2) (cadr sv-$2) sv-$1) ,dv-$1))

(define ($gen-res-fundef!-$2 sv-$1 sv-$2 sv-$3)
  ($gen-res-fundef-p!-$2
    (umainpe:find-name! '$eval-exp `(,sv-$1 ,sv-$2 ,sv-$3))
    sv-$1
    sv-$2
    sv-$3))

(define ($gen-res-fundef-p!-$2 fn sv-$1 sv-$2 sv-$3)
  (if (car fn)
    (umainpe:print-fundef! (cdr fn) '(sv) ($pe-exp-$8 sv-$1 sv-$2 sv-$3))
    (cdr fn)))

(define ($pe-exp-$7 sv-$1 sv-$2 dv-$1)
  (if (eq? (car sv-$2) sv-$1)
    `(car ,dv-$1)
    ($pe-exp-$7 sv-$1 (cdr sv-$2) `(cdr ,dv-$1))))

