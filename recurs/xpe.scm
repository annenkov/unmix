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

