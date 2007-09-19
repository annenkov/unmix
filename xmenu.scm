;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;  File:     xmenu.scm                                            ;;
;;  Project:  the specializer Unmix                                ;;
;;  Author:   S.A.Romanenko, the Institute for Applied             ;;
;;            Mathematics, the USSR Acedemy of Sciences,           ;;
;;            Moscow.                                              ;;
;;  Created:  11 April 1990                                        ;;
;;  Revised:  July 1990, December 1992, October 1993               ;;
;;                                                                 ;;
;;  Contents: The main function of Unmix.                          ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define upre:switch #f)
(define ugen:switch #f)
(define upost:switch #f)
(define uctmw:Scheme-to-Mixwell #f)
(define uctmwrl:rem-let-prog #f)
(define uctmwrl:cut-let-prog #f)
(define usepsd:unmix-static-and-dynamic #f)
(define ufcd:find-congruent-division #f)
(define uann:make-annotated-program #f)
(define uresfn:collect-residual-functions #f)
(define upiu:prevent-infinite-unfolding! #f)
(define upcd:prevent-call-duplication! #f)
(define umainpe:generate-residual-program #f)
(define xapply #f)
(define $specialize-fundef #f)
(define ucgr:main #f)
(define uar:main #f)
(define uarta:analyze-argument-types #f)
(define uaraa:analyze-parameter-access! #f)
(define uarps:optimize #f)
(define uensg:main #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;                      Main Function                              ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (unmix)
  (ux:main))

(define (ux:main)

  (set! upre:switch  #f)
  (set! ugen:switch  #f)
  (set! upost:switch #f)
  (set! uctmw:Scheme-to-Mixwell #f)
  (set! uctmwrl:rem-let-prog    #f)
  (set! uctmwrl:cut-let-prog    #f)
  (set! usepsd:unmix-static-and-dynamic   #f)
  (set! ufcd:find-congruent-division      #f)
  (set! uann:make-annotated-program       #f)
  (set! uresfn:collect-residual-functions #f)
  (set! upiu:prevent-infinite-unfolding!  #f)
  (set! upcd:prevent-call-duplication!    #f)
  (set! umainpe:generate-residual-program #f)
  (set! xapply  #f)
  (set! $specialize-fundef  #f)
  (set! ucgr:main  #f)
  (set! uar:main   #f)
  (set! uarta:analyze-argument-types    #f)
  (set! uaraa:analyze-parameter-access! #f)
  (set! uarps:optimize  #f)
  (set! uensg:main #f)
  (let ((reply
    (uio:display-menu
      '("U N M I X :   M a i n   m e n u")
      25
      '("Preprocessing"
        "Residual program generation"
        "pOstprocessing"
        ""
        "Compile SEX to SCM"
        "eValuating Scheme expression"
        ""
        "Quit Unmix"
        "eXit Scheme")
      "Work to do"
      '(P R O C V Q X))))
    (if (memq reply '(C V S Q))
        (uio:clear-screen))
    (case reply
      ((P) (ux:pre:main))
      ((R) (ux:gen:main))
      ((O) (ux:post:main))
      ((C) (ux:cmp:main) (uio:pause) (ux:main))
      ((V)
        (display "Enter an expression:") (newline)
        (let ((result (eval (read))))
          (do () ((not (char-ready?))) (read-char))
          (newline)(write result)
          (uio:pause)
          (ux:main)))
      ((Q)
       (display "Enter \"(unmix)\" to resume Unmix")(newline)
       'OK)
      ((X) (exit)))))

(define (ux:pre:main)
  (define (pre-switch action)
    (ux:load "xpre")
    (upre:switch action)
    (set! upre:switch #f)
    'OK)
  (let ((reply
    (uio:display-menu
      '("U N M I X :   P r e p r o c e s s i n g")
      6
      '("Preprocessing                 ann(desugar(s-prog),sdsd) -> ann-prog"
        ""
        "Desugaring                    desugar(s-prog)           -> mw-prog"
        "Annotating Mixwell program    ann(mw-prog,sdsd)         -> ann-prog"
        "Expanding macros              ensugar(desugar(s-prog))  -> s-prog"
        ""
        "Main menu")
      "Work to do"
      '(P D A E M))))
    (if (memq reply '(P D R A))
        (uio:clear-screen))
    (case reply
      ((P) (pre-switch 'pre) (uio:pause) (ux:main))
      ((D) (pre-switch 'dsg) (uio:pause) (ux:main))
      ((A) (pre-switch 'ann) (uio:pause) (ux:main))
      ((E) (pre-switch 'rmm) (uio:pause) (ux:main))
      (else (ux:main)))))

(define (ux:gen:main)
  (define (gen-switch action)
    (ux:load "xgen")
    (ugen:switch action)
    (set! ugen:switch #f)
    'OK)
  (let ((reply
    (uio:display-menu
      '("U N M I X :   G e n e r a t i o n")
      9
      '("Residual program generation   pe(ann-prog,statics) -> res-prog"
        "Self-application              pe(ann-pe,ann-prog)  -> gen"
        "Double self-application       pe(ann-pe,ann-pe)    -> gen-gen"
        "Generator generation          gen-gen(ann-prog)    -> gen"
        "Using program generator       gen(statics)         -> res-prog"
        ""
        "Main menu")
      "Work to do"
      '(R S D G U M))))
    (if (memq reply '(R S D G U))
        (uio:clear-screen))
      (case reply
        ((R) (gen-switch 'pe)     (uio:pause) (ux:main))
        ((S) (gen-switch 'pepe)   (uio:pause) (ux:main))
        ((D) (gen-switch 'pepepe) (uio:pause) (ux:main))
        ((G) (gen-switch 'gengen) (uio:pause) (ux:main))
        ((U) (gen-switch 'gen)    (uio:pause) (ux:main))
        (else (ux:main)))))

(define (ux:post:main)
  (define (post-switch action)
    (ux:load "xpost")
    (upost:switch action)
    (set! upost:switch #f)
    'OK)
  (let ((reply
    (uio:display-menu
      '("U N M I X :   P o s t p r o c e s s i n g")
      10
      '("Postprocessing                post(mw-prog)    -> s-prog"
        "Call graph reduction          cgr(mw-prog)     -> mw-prog"
        "Arity raising                 ar(mw-prog)      -> mw-prog"
        "Ensugaring                    ensugar(mw-prog) -> s-prog"
        "Formatting                    format(prog)     -> prog"
        ""
        "Main menu")
      "Work to do"
      '(P C A E F M))))
    (if (memq reply '(P C A E F))
        (uio:clear-screen))
    (case reply
      ((P) (post-switch 'post)    (uio:pause) (ux:main))
      ((C) (post-switch 'cgr)     (uio:pause) (ux:main))
      ((A) (post-switch 'ar)      (uio:pause) (ux:main))
      ((E) (post-switch 'ensugar) (uio:pause) (ux:main))
      ((F) (post-switch 'form)    (uio:pause) (ux:main))
      (else (ux:main)))))

(define (ux:cmp:main)
  (let* ((src
           (uio:request-file-name
             "Source Scheme program file name" "" "sex"))
         (file-name  (uio:cut-off-ext src))
         )
    (sex file-name)
    'OK))
