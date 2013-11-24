#lang racket
;; This file is used to run Unmix under Racket. 
(require "x-match.scm"
         "xmenu.scm" )

(for-each
 (lambda (x) (display x) (newline))
 '(""
   "UNMIX 3.0 (December 1993) for MacGambit Scheme"
   "   Sergei A.Romanenko, Keldysh Institute of Applied Mathematics,"
   "   Russian Academy of Sciences, Miusskaya Sq.4, 125047, Moscow, Russia"
   "")
 )

(display "Loading Unmix")(newline)
(unmix)