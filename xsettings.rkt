#lang racket

(define **unmix-path** "")

;; File extension for target scheme/racket programs.
(define **program-file-ext** ".rkt")

;; Set to #t to generate target programs as racket module
(define **output-as-racket-module** #t)

;; Racket-specific module header for target programs.
;; Affects output program only if **output-as-racket-module** is #t
(define **lang-directive** "#lang racket")

;; Switch UNMIX mode here
(define-for-syntax **mode** 'iterative)
;(define-for-syntax **mode** 'recursive)

(provide (all-defined-out)
         (for-syntax **mode**))
