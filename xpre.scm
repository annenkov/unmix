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
      (ux:load "xensg")
      (set! program (uensg:main pgm dst program))
      (set! uensg:main #f)
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
    (ux:load "xctmw")
    (set! prog (uctmw:compile-program prog))
    (set! uctmw:compile-program #f)
    (ux:load "xctmwrl")
    (set! prog (uctmwrl:rem-let-prog prog))
    (set! uctmwrl:rem-let-prog #f)
    (set! prog (uctmwrl:cut-let-prog prog))
    (set! uctmwrl:cut-let-prog #f)
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
    (ux:load "xresfn")
    (ux:load "xsepsd")
    (set! program
      (usepsd:unmix-static-and-dynamic program descr))
    (set! usepsd:unmix-static-and-dynamic #f)
    (newline)
    (ux:load "xpiu")
    (set! program
      (upiu:prevent-infinite-unfolding! program))
    (set! upiu:prevent-infinite-unfolding! #f)
    (newline)
    (ux:load "xpcd")
    (set! program
      (upcd:prevent-call-duplication! program))
    (set! upcd:prevent-call-duplication! #f)
    (set! uresfn:collect-residual-functions #f)
    program)
  (define (request-descr)
    (map (lambda (x)
           (string->symbol
             (make-string 1 (char-downcase x))))
         (string->list
           (uio:request-string "Parameter description: "))))
  (define (check-descr mw-prog descr)
    (if (not (is-sd-list? descr))
      (begin
        (error "Malformed description of the input parameters"
               descr)))
    (let ((- (cdr mw-prog)) (parlist (cadar mw-prog)))
      (if (not (eqv? (length parlist) (length descr)))
        (begin
          (error "Incorrect number of indicators in the description"
                 descr))))
    (if (not (memq 'd descr))
      (begin
        (error "No \"d\" in the description of the input parameters"
               descr)))
    'ok)
  (define (is-sd-list? descr)
    (and-map
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

