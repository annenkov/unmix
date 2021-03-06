#lang racket
(require racket/mpair)
(require "xresfn.rkt"
         "x-misc.rkt")

(define (upiu:prevent-infinite-unfolding! prog)
  (define (find-loops-prog rf prog)
    (define loops #f)
    (define (find-loops-func fname)
      (let ((%%1 (assq fname prog)))
        (let ((body (car (cddddr %%1))))
          (find-loops body fname '() (list fname) '()))))
    (define (find-loops exp fn trace fn-path path)
      (cond ((symbol? exp) #f)
            ((equal? (car exp) 'static)
             (let ((exp1 (cadr exp))) #f))
            ((equal? (car exp) 'ifs)
             (let ((exp* (cddr exp)) (exp0 (cadr exp)))
               (find-loops* exp* 1 fn trace fn-path path)))
            ((equal? (car exp) 'ifd)
             (let ((exp* (cdr exp)))
               (find-loops* exp* 0 fn trace fn-path path)))
            ((equal? (car exp) 'call)
             (let ((d-exp* (cadddr exp))
                   (s-exp* (caddr exp))
                   (fname (cadr exp)))
               (find-loops* d-exp* 0 fn trace fn-path path)
               (if (memq fname fn-path)
                 (record-loop fname fn trace path)
                 (visit-function fname fn trace fn-path path))))
            ((equal? (car exp) 'rcall)
             (let ((d-exp* (cadddr exp))
                   (s-exp* (caddr exp))
                   (fname (cadr exp)))
               (find-loops* d-exp* 0 fn trace fn-path path)))
            ((equal? (car exp) 'xcall)
             (let ((exp* (cddr exp)) (fname (cadr exp)))
               (find-loops* exp* 0 fn trace fn-path path)))
            (else
             (let ((exp* (cdr exp)) (op (car exp)))
               (find-loops* exp* 0 fn trace fn-path path)))))
    (define (find-loops* exp* num fn trace fn-path path)
      (if (null? exp*)
        #f
        (let ((rest (cdr exp*)) (exp (car exp*)))
          (find-loops
            exp
            fn
            `(,num unquote trace)
            fn-path
            path)
          (find-loops*
            rest
            (+ num 1)
            fn
            trace
            fn-path
            path))))
    (define (record-loop fname fn trace path)
      (let ((new-loop
              `((,fn unquote (reverse trace))
                unquote
                (extract-loop
                  fname
                  `((,fn unquote trace) unquote path)
                  '()))))
        (when (not (member new-loop loops))
          (begin (set! loops `(,new-loop unquote loops))))))
    (define (extract-loop fname path loop)
      (let ((path-rest (cdr path))
            (trace1 (cdar path))
            (fname1 (caar path)))
        (let ((%%2 `((,fname1 unquote (reverse trace1)) unquote loop)))
          (let ((loop %%2))
            (if (eq? fname fname1)
              loop
              (extract-loop fname path-rest loop))))))
    (define (visit-function fname fn trace fn-path path)
      (let ((%%3 (assq fname prog)))
        (let ((body (car (cddddr %%3))))
          (find-loops
            body
            fname
            '()
            `(,fname unquote fn-path)
            `((,fn unquote trace) unquote path)))))
    (set! loops '())
    (for-each
      (lambda (fname) (find-loops-func fname))
      rf)
    loops)
  (define (collect-dangerous-calls loops prog)
    (define dangerous-calls #f)
    (define (collect-in-fundef loop)
      (let ((path (cdr loop)) (back-call (car loop)))
        (cond ((member back-call dangerous-calls) #f)
              ((dangerous-path? path)
               (set! dangerous-calls
                 `(,back-call unquote dangerous-calls)))
              (else #f))))
    (define (dangerous-path? path)
      (let ((path-rest (cdr path))
            (trace (cdar path))
            (fname (caar path)))
        (let ((%%4 (assq fname prog)))
          (let ((body (car (cddddr %%4)))
                (dvn (caddr %%4))
                (svn (cadr %%4)))
            (let ((%%5 (go-through-path
                         body
                         trace
                         path-rest
                         svn
                         (make-le svn))))
              (let ((new-svv %%5))
                (or (not (all-non-increasing? svn new-svv))
                    (not (some-decreasing? new-svv)))))))))
    (define (go-through-path exp trace path vn vv)
      (cond ((symbol? exp)
             (error "No way in the expression" exp))
            ((equal? (car exp) 'static)
             (error "No way in the expression" exp))
            ((equal? (car exp) 'call)
             (let ((d-exp* (cadddr exp))
                   (s-exp* (caddr exp))
                   (fname (cadr exp)))
               (if (null? trace)
                 (go-through-call
                   fname
                   (decr-eval* s-exp* vn vv)
                   path)
                 (let ((trace-rest (cdr trace)) (num (car trace)))
                   (go-through-path
                     (list-ref d-exp* num)
                     trace-rest
                     path
                     vn
                     vv)))))
            ((equal? (car exp) 'rcall)
             (let ((d-exp* (cadddr exp))
                   (s-exp* (caddr exp))
                   (fname (cadr exp)))
               (let ((trace-rest (cdr trace)) (num (car trace)))
                 (go-through-path
                   (list-ref d-exp* num)
                   trace-rest
                   path
                   vn
                   vv))))
            ((equal? (car exp) 'xcall)
             (let ((exp* (cddr exp)) (fname (cadr exp)))
               (let ((trace-rest (cdr trace)) (num (car trace)))
                 (go-through-path
                   (list-ref exp* num)
                   trace-rest
                   path
                   vn
                   vv))))
            (else
             (let ((exp* (cdr exp)) (op (car exp)))
               (let ((trace-rest (cdr trace)) (num (car trace)))
                 (go-through-path
                   (list-ref exp* num)
                   trace-rest
                   path
                   vn
                   vv))))))
    (define (go-through-call fname svv path)
      (if (null? path)
        svv
        (let ((path-rest (cdr path)) (path-head (car path)))
          (let ((%%6 (assq fname prog)))
            (let ((trace (cdr path-head))
                  (body (car (cddddr %%6)))
                  (dvn (caddr %%6))
                  (svn (cadr %%6)))
              (go-through-path body trace path-rest svn svv))))))
    (define (decr-eval exp vn vv)
      (cond ((symbol? exp) (lookup-variable exp vn vv))
            ((equal? (car exp) 'quote) 'any)
            ((let ((exp* (cdr exp)) (op (car exp)))
               (memq op '(car cdr)))
             (let ((exp* (cdr exp)) (op (car exp)))
               (decr-eval-sel (decr-eval (car exp*) vn vv))))
            (else 'any)))
    (define (decr-eval* exp* vn vv)
      (map (lambda (exp) (decr-eval exp vn vv)) exp*))
    (define (decr-eval-sel a-value)
      (cond ((equal? a-value 'any) 'any)
            ((equal? (car a-value) 'lt)
             (let ((vname (cdr a-value))) a-value))
            ((equal? (car a-value) 'le)
             (let ((vname (cdr a-value))) `(lt unquote vname)))
            (else (error "SELECT: no match for" a-value))))
    (define (make-le vn)
      (map (lambda (vname) `(le unquote vname)) vn))
    (define (all-non-increasing? vn vv)
      (if (and (null? vn) (null? vv))
        #t
        (let ((vv-rest (cdr vv))
              (vvalue (car vv))
              (vn-rest (cdr vn))
              (vname (car vn)))
          (cond ((equal? vvalue 'any) #f)
                ((equal? (car vvalue) 'lt)
                 (let ((vname1 (cdr vvalue)))
                   (and (eq? vname vname1)
                        (all-non-increasing? vn-rest vv-rest))))
                ((equal? (car vvalue) 'le)
                 (let ((vname1 (cdr vvalue)))
                   (and (eq? vname vname1)
                        (all-non-increasing? vn-rest vv-rest))))
                (else (error "SELECT: no match for" vvalue))))))
    (define (some-decreasing? vv)
      (or-map
        (lambda (vvalue)
          (cond ((equal? (car vvalue) 'lt) #t)
                ((equal? (car vvalue) 'le) #f)
                (else (error "SELECT: no match for" vvalue))))
        vv))
    (define (lookup-variable vname vn vv)
      (if (and (null? vn) (null? vv))
        (error "Undefined variable: " vname)
        (let ((vrest (cdr vv))
              (vv (car vv))
              (nrest (cdr vn))
              (vn (car vn)))
          (if (eq? vname vn)
            vv
            (lookup-variable vname nrest vrest)))))
    (set! dangerous-calls '())
    (for-each collect-in-fundef loops)
    dangerous-calls)
  (define (mark-dangerous-calls! prog dangerous-calls)
    (define (mark-dc-fundef! fname trace prog)
      (let ((%%7 (massq fname prog)))
        (let ((body (mcar (mcddddr %%7))))
          (mark-dc! body trace))))
    (define (mark-dc! exp trace)
      (cond ((symbol? exp)
             (error "No way in the expression: " exp))
            ((equal? (mcar exp) 'static)
             (let ((exp1 (mcadr exp)))
               (error "No way in the expression: " exp)))
            ((let ((&call (mcar exp)))
               (memq &call '(call rcall)))
             (let ((&call (mcar exp)))
               (let ((d-exp* (mcadddr exp))
                     (s-exp* (mcaddr exp))
                     (fname (mcadr exp)))
                 (if (null? trace)
                   (set-mcar! exp 'rcall)
                   (let ((trace-rest (cdr trace)) (num (car trace)))
                     (mark-dc! (mlist-ref d-exp* num) trace-rest))))))
            ((equal? (mcar exp) 'xcall)
             (let ((exp* (mcddr exp)) (fname (mcadr exp)))
               (let ((trace-rest (cdr trace)) (num (car trace)))
                 (mark-dc! (list-ref exp* num) trace-rest))))
            (else
             (let ((exp* (mcdr exp)) (op (mcar exp)))
               (let ((trace-rest (cdr trace)) (num (car trace)))
                 (mark-dc! (mlist-ref exp* num) trace-rest))))))
    (for-each
      (lambda (back-call)
        (let ((trace (cdr back-call)) (fname (car back-call)))
          (mark-dc-fundef! fname trace prog)))
      dangerous-calls))
  (display "Preventing Infinite Unfolding")
  (newline)
  (let ((s-fundef* (caddr prog))
        (d-fundef* (cadr prog))
        (d-fundef** (pairs->mpairs (cadr prog))) ; converting to mutable
        (rf (car prog)))
    (display "Finding Loops")
    (newline)
    (let* ((loops (find-loops-prog rf d-fundef*))
           (dangerous-calls
             (collect-dangerous-calls loops d-fundef*)))
      (display "Dangerous calls:")
      (newline)
      (write dangerous-calls)
      (newline)
      (display "Cutting Dangerous Loops")
      (newline)
      (mark-dangerous-calls! d-fundef** dangerous-calls)
      (let* ((rf (uresfn:collect-residual-functions d-fundef*)))
        (display "-- Done --")
        (newline)
        `(,rf ,d-fundef** ,s-fundef*)))))


(provide (all-defined-out))