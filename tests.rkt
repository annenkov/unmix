#lang racket
(require rackunit
         rackunit/text-ui)
(require "x-macro.rkt"
         "xmainpe.rkt"
         "xctmw.rkt"
         "xctmwrl.rkt"
         "xsepsd.rkt"
         "xpiu.rkt"
         "xpcd.rkt"
         "x-misc.rkt"
         "xcgr.rkt"
         "xar.rkt"
         "xcgr.rkt"
         "xensg.rkt")

; Source program on Scheme with EXtensions
(define prog-sex
  '((define (test1 x)
      (rcall (test2 x)))
    (define (test2 x)
      (generalize x))))

; simple sex->rkt test: 'generalize' and 'rcall' should be removed from prog
(check-equal? (map ux:macroexpand prog-sex)
              '((define (test1 x)
                  (test2 x))
                (define (test2 x)
                  x)))

(define prog-zip
  '((define (start x y)
      (zipper x y))
    
    (define (zipper x y)
      (cond ((null? x) y)
            ((null? y) x)
            (else
             `(,(car x) ,(car y) . ,(zipper (cdr x) (cdr y))))))))

(define zip-mw
    (uctmwrl:cut-let-prog
     (uctmwrl:rem-let-prog 
      (uctmw:compile-program prog-zip))))    

(define zip-ann
  (mpairs->pairs* 
   (upcd:prevent-call-duplication!
    (upiu:prevent-infinite-unfolding!
     (usepsd:unmix-static-and-dynamic zip-mw '(s d))))))

(define zip123-mw 
  (let ([o-port (open-output-string "")])
    (umainpe:generate-residual-program* o-port zip-ann '((1111 2222 3333)))
    (read (open-input-string (get-output-string o-port)))))

(define zip123
  (uensg:main "zip" "zip"
   (ucgr:main "zip" "zip"
    (uar:main "zip" "zip" (ucgr:main "zip" "zip" (list zip123-mw))))))

(check-equal? 
 (car zip123)
 '(define (start-$1 y)
    (if (null? y)
        '(1111 2222 3333)
        `(1111
          ,(car y)
          unquote
          (if (null? (cdr y))
              '(2222 3333)
              `(2222
                ,(cadr y)
                unquote
                (if (null? (cddr y))
                    '(3333)
                    `(3333 ,(caddr y) unquote (cdddr y)))))))))