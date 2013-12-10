#lang racket
(require racket/mpair)
(require "../x-misc.rkt")

(define umainpe:o-port '())

(define umainpe:names '())

(define umainpe:counts '())

(define (umainpe:generate-residual-program o-file ann-prog goalsv [gen #f])
  (set! umainpe:o-port (open-output-file o-file))
  (set! umainpe:counts '())
  (set! umainpe:names '())
  (if (null? ann-prog) ($start goalsv) ($specialize-fundef ann-prog goalsv))
  (set! umainpe:counts '())
  (set! umainpe:names '())
  (close-output-port umainpe:o-port))

(define xapply
  (let ((proc-list '()))
    (lambda (fname args)
      (let ((fname/proc (assq fname proc-list)))
        (if fname/proc
          (apply (cdr fname/proc) args)
          (let ((proc (eval fname)))
            (set! proc-list `((,fname unquote proc) unquote proc-list))
            (apply proc args)))))))

;;;;;;;;; Form xggg.rkt ;;;;;;;;;;

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

(define (umainpe:print-fundef! fname var body)
  (display ">")
  (write `(,fname ,var = ,body) umainpe:o-port)
  (newline umainpe:o-port)
  (newline umainpe:o-port)
  fname)

(define (umainpe:find-name! fname sv)
  (define (make-fname! fname)
    (string->symbol
      (string-append
        (symbol->string fname)
        "-$"
        (number->string (get-free-count! fname)))))
  (define (get-free-count! fname)
    (let ((fname-descr (massq fname umainpe:counts)))
      (if fname-descr
        (let ((count (+ 1 (mcdr fname-descr))))
          (set-mcdr! fname-descr count)
          count)
        (begin
          (set! umainpe:counts (pairs->mpairs `((,fname . 1) unquote umainpe:counts))) ; converting to mutable
          1))))
  (let* ((conf `(,fname unquote sv))
         (conf-descr (assoc conf umainpe:names))
         (bool (not conf-descr)))
    (when bool
      (begin
        (set! conf-descr `(,conf unquote (make-fname! fname)))
        (set! umainpe:names `(,conf-descr unquote umainpe:names))
        (display "<")))
    `(,bool unquote (cdr conf-descr))))


;;;;;;;;; Form xpe.rkt ;;;;;;;;;;

(define ($specialize-fundef ann-prog sv)
  (let ((s-prog (caddr ann-prog))
        (d-prog (cadr ann-prog))
        (goal (caar ann-prog)))
    (let ((%%1 (assq goal d-prog)))
      (let ((body (car (cddddr %%1))) (dn (caddr %%1)) (sn (cadr %%1)))
        (let ((%%2 (umainpe:find-name! goal sv)))
          (let ((rfname (cdr %%2)))
            (list (umainpe:print-fundef! '$start dn `(call ,rfname unquote dn))
                  (umainpe:print-fundef!
                    rfname
                    dn
                    ($pe-exp body
                             sn
                             dn
                             ($contract-sv sn sv)
                             dn
                             s-prog
                             d-prog)))))))))

(define ($gen-res-fundef! fname sv s-prog d-prog)
  (let ((%%3 (assq fname d-prog)))
    (let ((body (car (cddddr %%3))) (dn (caddr %%3)) (sn (cadr %%3)))
      (let ((%%4 (umainpe:find-name! fname ($contract-sv sn sv))))
        (let ((fn %%4))
          ($gen-res-fundef-p! fn body sn dn sv s-prog d-prog))))))

(define ($gen-res-fundef-p! fn body sn dn sv s-prog d-prog)
  (let ((rfname (cdr fn)) (new (car fn)))
    (if new
      (umainpe:print-fundef!
        rfname
        dn
        ($pe-exp body sn dn sv dn s-prog d-prog))
      rfname)))

(define ($contract-sv sn sv)
  (if (null? sn)
    '()
    (let ((rest (cdr sn))) `(,(car sv) unquote ($contract-sv rest (cdr sv))))))

(define ($pe-exp exp sn dn sv dv s-prog d-prog)
  (cond ((let ((vname exp)) (not (pair? vname)))
         (let ((vname exp)) ($lookup-value vname dn dv)))
        ((equal? (car exp) 'static)
         (let ((exp1 (cadr exp))) `',($eval-exp exp1 sn sv s-prog)))
        ((equal? (car exp) 'ifs)
         (let ((exp3 (cadddr exp)) (exp2 (caddr exp)) (exp1 (cadr exp)))
           (if ($eval-exp exp1 sn sv s-prog)
             ($pe-exp exp2 sn dn sv dv s-prog d-prog)
             ($pe-exp exp3 sn dn sv dv s-prog d-prog))))
        ((equal? (car exp) 'ifd)
         (let ((exp3 (cadddr exp)) (exp2 (caddr exp)) (exp1 (cadr exp)))
           `(if ,($pe-exp exp1 sn dn sv dv s-prog d-prog)
              ,($pe-exp exp2 sn dn sv dv s-prog d-prog)
              ,($pe-exp exp3 sn dn sv dv s-prog d-prog))))
        ((equal? (car exp) 'call)
         (let ((d-exp* (cadddr exp)) (s-exp* (caddr exp)) (fname (cadr exp)))
           ($pe-call
             (assq fname d-prog)
             ($eval-exp* s-exp* sn sv s-prog)
             ($pe-exp* d-exp* sn dn sv dv s-prog d-prog)
             s-prog
             d-prog)))
        ((equal? (car exp) 'rcall)
         (let ((d-exp* (cadddr exp)) (s-exp* (caddr exp)) (fname (cadr exp)))
           (let ((%%5 ($eval-exp* s-exp* sn sv s-prog)))
             (let ((s-val* %%5))
               (let ((%%6 ($gen-res-fundef! fname s-val* s-prog d-prog)))
                 (let ((rfname %%6))
                   `(call ,rfname
                          unquote
                          ($pe-exp* d-exp* sn dn sv dv s-prog d-prog))))))))
        ((equal? (car exp) 'xcall)
         (let ((exp* (cddr exp)) (fname (cadr exp)))
           `(xcall ,fname unquote ($pe-exp* exp* sn dn sv dv s-prog d-prog))))
        (else
         (let ((exp* (cdr exp)) (fname (car exp)))
           `(,fname unquote ($pe-exp* exp* sn dn sv dv s-prog d-prog))))))

(define ($pe-exp* exp* sn dn sv dv s-prog d-prog)
  (if (null? exp*)
    '()
    (let ((rest (cdr exp*)) (exp (car exp*)))
      `(,($pe-exp exp sn dn sv dv s-prog d-prog)
        unquote
        ($pe-exp* rest sn dn sv dv s-prog d-prog)))))

(define ($pe-call fundef sv dv s-prog d-prog)
  (let ((body (car (cddddr fundef))) (dn (caddr fundef)) (sn (cadr fundef)))
    ($pe-exp body sn dn sv dv s-prog d-prog)))

(define ($eval-exp exp sn sv prog)
  (cond ((let ((vname exp)) (not (pair? vname)))
         (let ((vname exp)) ($lookup-value vname sn sv)))
        ((equal? (car exp) 'quote) (let ((s-exp (cadr exp))) s-exp))
        ((equal? (car exp) 'if)
         (let ((exp3 (cadddr exp)) (exp2 (caddr exp)) (exp1 (cadr exp)))
           (if ($eval-exp exp1 sn sv prog)
             ($eval-exp exp2 sn sv prog)
             ($eval-exp exp3 sn sv prog))))
        ((equal? (car exp) 'call)
         (let ((exp* (cddr exp)) (fname (cadr exp)))
           ($eval-call prog (assq fname prog) ($eval-exp* exp* sn sv prog))))
        ((equal? (car exp) 'xcall)
         (let ((exp* (cddr exp)) (fname (cadr exp)))
           (xapply fname ($eval-exp* exp* sn sv prog))))
        ((equal? (car exp) 'error)
         (let ((exp* (cdr exp)))
           (error "Error function encountered during partial evaluation"
                  `(error unquote ($eval-exp* exp* sn sv prog)))))
        ((equal? (car exp) 'car)
         (let ((exp1 (cadr exp))) (car ($eval-exp exp1 sn sv prog))))
        ((equal? (car exp) 'cdr)
         (let ((exp1 (cadr exp))) (cdr ($eval-exp exp1 sn sv prog))))
        ((equal? (car exp) 'cons)
         (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
           (cons ($eval-exp exp1 sn sv prog) ($eval-exp exp2 sn sv prog))))
        ((equal? (car exp) 'null?)
         (let ((exp1 (cadr exp))) (null? ($eval-exp exp1 sn sv prog))))
        ((equal? (car exp) 'pair?)
         (let ((exp1 (cadr exp))) (pair? ($eval-exp exp1 sn sv prog))))
        ((equal? (car exp) 'equal?)
         (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
           (equal? ($eval-exp exp1 sn sv prog) ($eval-exp exp2 sn sv prog))))
        ((equal? (car exp) 'eq?)
         (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
           (eq? ($eval-exp exp1 sn sv prog) ($eval-exp exp2 sn sv prog))))
        ((equal? (car exp) 'eqv?)
         (let ((exp2 (caddr exp)) (exp1 (cadr exp)))
           (eqv? ($eval-exp exp1 sn sv prog) ($eval-exp exp2 sn sv prog))))
        ((equal? (car exp) 'not)
         (let ((exp1 (cadr exp))) (not ($eval-exp exp1 sn sv prog))))
        (else
         (let ((exp* (cdr exp)) (fname (car exp)))
           (xapply fname ($eval-exp* exp* sn sv prog))))))

(define ($eval-exp* exp* sn sv prog)
  (if (null? exp*)
    '()
    (let ((rest (cdr exp*)) (exp (car exp*)))
      `(,($eval-exp exp sn sv prog) unquote ($eval-exp* rest sn sv prog)))))

(define ($eval-call prog fundef sv)
  (let ((body (cadddr fundef)) (sn (cadr fundef)))
    ($eval-exp body sn sv prog)))

(define ($lookup-value vname vn vv)
  (let ((vvtl (cdr vv)) (vvhd (car vv)) (vntl (cdr vn)) (vnhd (car vn)))
    (if (eq? vnhd vname) vvhd ($lookup-value vname vntl vvtl))))

(provide (all-defined-out))
