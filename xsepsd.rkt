#lang racket
(require "xfcd.rkt"
         "xann.rkt"
         "x-misc.rkt")

(define (usepsd:unmix-static-and-dynamic mw-prog descr)
  (define (check-input-division descr mc)
    (let* ([mc* (mpairs->pairs mc)]
           [fres (cddar mc*)]
           [fargs (cadar mc*)]
           [fname (caar mc*)])
      (when (not (equal? descr fargs))
        (begin
          (display "The division of the program's input parameters")
          (newline)
          (display "obtained by the abstract interpretation is")
          (newline)
          (write fargs)
          (newline)
          (display "which contradicts to the division")
          (newline)
          (write descr)
          (newline)
          (display "prescribed by the user.")
          (newline)
          (error "")))))
  (display "Finding Congruent Division")
  (newline)
  (display "Iterations: ")
  (let ((mc (ufcd:find-congruent-division mw-prog descr)))
    ; TODO check later
    ;(set! ufcd:find-congruent-division #f)
    (newline)
    (display "Unmixing Static and Dynamic Data")
    (newline)
    (check-input-division descr mc)    
    (let ((ann-prog (uann:make-annotated-program mw-prog mc)))
      ; TODO check later
      ;(set! uann:make-annotated-program #f)
      (display "-- Done --")
      (newline)
      ann-prog)))

(provide (all-defined-out))