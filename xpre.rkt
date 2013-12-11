#lang racket
(require "xio.rkt"
         "xensg.rkt"
         "xctmw.rkt"
         "xctmwrl.rkt"
         "xresfn.rkt"
         "xsepsd.rkt"
         "xpiu.rkt"
         "xpcd.rkt")

(define (upre:switch action)
  (define (run-pre)
    (let* ((src (uio:request-file-name
                  "Scheme program file name"
                  ""
                  "sex"))
           (pgm (uio:cut-off-ext src))
           (dst (string-append pgm ".ann"))
           (descr (request-descr))
           (program #f))
      (newline)
      (display "-- Pre-processing: ")
      (display src)
      (display " -> ")
      (display dst)
      (newline)
      (set! program (uio:file->list src))
      (set! program
        (scheme-to-mixwell pgm pgm program))
      (set! program (annotate pgm pgm descr program))
      (write-ann-prog dst program)
      (newline)
      (display "Target program has been written into ")
      (display dst)
      (newline)))
  (define (run-dsg)
    (let* ((src (uio:request-file-name
                  "Source Scheme program file name"
                  ""
                  "sex"))
           (dst (uio:request-file-name
                  "Target Mixwell program file name"
                  src
                  "mw"))
           (program (uio:file->list src)))
      (set! program
        (scheme-to-mixwell src dst program))
      (uio:list->pp-file dst program 79)
      (newline)
      (display
        "Desugared program has been written into ")
      (display dst)
      (newline)))
  (define (run-rmm)
    (let* ((src (uio:request-file-name
                  "Source Scheme program file name"
                  ""
                  "sex"))
           (pgm (uio:cut-off-ext src))
           (dst (uio:request-file-name
                  "Target Scheme program file name"
                  src
                  "scm"))
           (program (uio:file->list src)))
      (set! program
        (scheme-to-mixwell src pgm program))      
      (set! program (uensg:main pgm dst program))
      ;(set! uensg:main #f)
      (uio:list->pp-file dst program 79)
      (newline)
      (display "Target program has been written into ")
      (display dst)
      (newline)))
  (define (scheme-to-mixwell src dst prog)
    (newline)
    (display "-- Desugaring: ")
    (display src)
    (display " -> ")
    (display dst)
    (newline)
    (set! prog (uctmw:compile-program prog))
    (set! prog (uctmwrl:rem-let-prog prog))
    (set! prog (uctmwrl:cut-let-prog prog))
    (display "-- Done --")
    (newline)
    prog)
  (define (run-ann)
    (let* ((src (uio:request-file-name
                  "Mixwell program file name"
                  ""
                  "mw"))
           (pgm (uio:cut-off-ext src))
           (dst (string-append pgm ".ann"))
           (descr (request-descr))
           (program (uio:file->list src)))
      (set! program (annotate src dst descr program))
      (write-ann-prog dst program)
      (newline)
      (display "Target program has been written into ")
      (display dst)
      (newline)))
  (define (annotate src dst descr program)
    (check-descr program descr)
    (newline)
    (display "-- Annotating: ")
    (display src)
    (display " -> ")
    (display dst)
    (newline)
    (newline)
    (set! program
      (usepsd:unmix-static-and-dynamic program descr))
    (newline)
    (set! program
      (upiu:prevent-infinite-unfolding! program))
    (newline)
    (set! program
      (upcd:prevent-call-duplication! program))
    program)
  (define (request-descr)
    (map (lambda (x)
           (string->symbol
             (make-string 1 (char-downcase x))))
         (string->list
           (uio:request-string "Parameter description: "))))
  (define (check-descr mw-prog descr)
    (when (not (is-sd-list? descr))
      (begin
        (error "Malformed description of the input parameters"
               descr)))
    (let ((- (cdr mw-prog)) (parlist (cadar mw-prog)))
      (when (not (eqv? (length parlist) (length descr)))
        (begin
          (error "Incorrect number of indicators in the description"
                 descr))))
    (when (not (memq 'd descr))
      (begin
        (error "No \"d\" in the description of the input parameters"
               descr)))
    'ok)
  (define (is-sd-list? descr)
    (andmap
      (lambda (item) (memq item '(s d)))
      descr))
  (define (write-ann-prog dst ann-prog)
    (let ((s-fndef* (caddr ann-prog))
          (d-fndef* (cadr ann-prog))
          (rf-names (car ann-prog)))
      (let ((%%1 (open-output-file dst)))
        (let ((p %%1))
          (write rf-names p)
          (newline p)
          (display "(" p)
          (newline p)
          (uio:pp-list d-fndef* p)
          (display ")(" p)
          (newline p)
          (uio:pp-list s-fndef* p)
          (newline p)
          (display ")" p)
          (close-output-port p)))))
  (case action
    ((pre) (run-pre))
    ((dsg) (run-dsg))
    ((rmm) (run-rmm))
    ((ann) (run-ann))))

(provide (all-defined-out))