(define umainpe:o-port '())

(define umainpe:names '())

(define umainpe:counts '())

(define (umainpe:generate-residual-program o-file ann-prog goalsv)
  (set! umainpe:o-port (open-output-file o-file))
  (set! umainpe:counts '())
  (set! umainpe:names '())
  (if (null? ann-prog) ($start goalsv) ($specialize-fundef ann-prog goalsv))
  (set! umainpe:counts '())
  (set! umainpe:names '())
  (close-output-port umainpe:o-port))

(define (umainpe:find-name! fname sv)
  (define (make-fname! fname)
    (string->symbol
      (string-append
        (symbol->string fname)
        "-$"
        (number->string (get-free-count! fname)))))
  (define (get-free-count! fname)
    (let ((fname-descr (assq fname umainpe:counts)))
      (if fname-descr
        (let ((count (+ 1 (cdr fname-descr))))
          (set-cdr! fname-descr count)
          count)
        (begin
          (set! umainpe:counts `((,fname . 1) unquote umainpe:counts))
          1))))
  (let* ((conf `(,fname unquote sv))
         (conf-descr (assoc conf umainpe:names))
         (bool (not conf-descr)))
    (if bool
      (begin
        (set! conf-descr `(,conf unquote (make-fname! fname)))
        (set! umainpe:names `(,conf-descr unquote umainpe:names))
        (display "<")))
    `(,bool unquote (cdr conf-descr))))

(define (umainpe:print-fundef! fname var body)
  (display ">")
  (write `(,fname ,var = ,body) umainpe:o-port)
  (newline umainpe:o-port)
  (newline umainpe:o-port)
  fname)

(define xapply
  (let ((proc-list '()))
    (lambda (fname args)
      (let ((fname/proc (assq fname proc-list)))
        (if fname/proc
          (apply (cdr fname/proc) args)
          (let ((proc (eval fname)))
            (set! proc-list `((,fname unquote proc) unquote proc-list))
            (apply proc args)))))))

