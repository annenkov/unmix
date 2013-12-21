#lang racket
(require rackunit
         rackunit/text-ui)
(require "x-macro.rkt")

(define prog
  '((define (test1 x)
      (rcall (test2 x)))
    (define (test2 x)
      (generalize x))))

; simple sex->rkt test: 'generalize' and 'rcall' should be removed from prog
(check-equal? (map ux:macroexpand prog)
              '((define (test1 x)
                  (test2 x))
                (define (test2 x)
                  x)))
