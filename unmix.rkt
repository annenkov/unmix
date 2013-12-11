#lang racket
;; This file is used to run Unmix under Racket. 
(require "x-match.rkt"
         "xmenu.rkt" )

(for-each
 (lambda (x) (display x) (newline))
 '(""
   "UNMIX 4.0 (December 2013) for Racket" 
   "(based on UNMIX 3.0: https://code.google.com/p/unmix/)"
   "   Sergei A.Romanenko, Keldysh Institute of Applied Mathematics,"
   "   Russian Academy of Sciences, Miusskaya Sq.4, 125047, Moscow, Russia"
   "   Ported to Racket by Danil Annenkov (annekov@ib-soft.ru)"
   "")
 )

(display "Loading Unmix")(newline)
(unmix)