#lang racket
(require "xarta.rkt"
         "xaraa.rkt"
         "xarps.rkt"
         "x-misc.rkt")

(define (uar:main src dst prog)
  (define types #f)
  (define (print-types types) (write (form-types types)) (newline))
  (define (form-types types)
    (map (lambda (fdescr)
           (let ((type* (cdr fdescr)) (fname (car fdescr)))
             `(,fname unquote (map form-type type*))))
         types))
  (define (form-type type)
    (cond ((equal? type 'absent) '_)
          ((equal? type 'any) '_)
          ((equal? (car type) 'atom) (let ((a (cadr type))) a))
          ((equal? (car type) 'cons)
           (let ((t2 (caddr type)) (t1 (cadr type)))
             `(,(form-type t1) unquote (form-type t2))))
          (else (error "SELECT: no match for" type))))
  (newline)
  (display "-- Arity Raising:  ")
  (display src)
  (display " -> ")
  (display dst)
  (newline)
  (newline)
  (display "Analysis of the Argument Types")
  (newline)
  (display "Iterations: ")
  (set! types (mpairs->pairs (uarta:analyze-argument-types prog))) ; converting back to regular pairs
  ; TODO Check later
  ;(set! uarta:analyze-argument-types #f)
  (newline)
  (display "Structure of Arguments:")
  (newline)
  (print-types types)
  (display "Analysis of the Parameter Accesses")
  (newline)  
  (display "Iterations: ")
  (uaraa:analyze-parameter-access! prog (pairs->mpairs types)) ; converting to mutable
  ; TODO Check later
  ;(set! uaraa:analyze-parameter-access! #f)
  (newline)
  (display "Structure of Arguments:")
  (newline)
  (print-types types)
  (display "Splitting of Parameters")
  (newline)
  (set! prog (uarps:optimize prog types))
  ; TODO Check later
  ;(set! uarps:optimize #f)
  (display "-- Done --")
  (newline)
  prog)


(provide (all-defined-out))