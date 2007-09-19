(define (upost:switch action)
  (define (run-post)
    (newline)
    (let* ((src (uio:request-file-name "Mixwell program file name" "" "MW"))
           (dst (uio:request-file-name "Scheme program file name" src "SCM"))
           (program #f))
      (let ((pgm (uio:cut-off-ext src)))
        (newline)
        (display "Post-processing:")
        (newline)
        (display "   post( ")
        (display src)
        (display " ) -> ")
        (display dst)
        (newline)
        (set! program (uio:file->list src))
        (ux:load "xcgr")
        (set! program (ucgr:main pgm pgm program))
        (set! ucgr:main #f)
        (ux:load "xar")
        (set! program (uar:main pgm pgm program))
        (set! uar:main #f)
        (ux:load "xcgr")
        (set! program (ucgr:main pgm pgm program))
        (set! ucgr:main #f)
        (ux:load "xensg")
        (set! program (uensg:main pgm pgm program))
        (set! uensg:main #f)
        (uio:list->pp-file dst program 79)
        (newline)
        (display "Target program has been written into ")
        (display dst)
        (newline))))
  (define (run-cgr)
    (newline)
    (let* ((src (uio:request-file-name
                  "Source Mixwell program file name"
                  ""
                  "MW"))
           (dst (uio:request-file-name
                  "Target Mixwell program file name"
                  src
                  "CGR"))
           (prog (uio:file->list src)))
      (ux:load "xcgr")
      (set! prog (ucgr:main src dst prog))
      (set! ucgr:main #f)
      (uio:list->pp-file dst prog 79)
      (newline)
      (display "Target program has been written into ")
      (display dst)
      (newline)))
  (define (run-ar)
    (newline)
    (let* ((src (uio:request-file-name
                  "Source Mixwell program file name"
                  ""
                  "MW"))
           (dst (uio:request-file-name
                  "Target Mixwell program file name"
                  src
                  "AR"))
           (prog (uio:file->list src)))
      (ux:load "xar")
      (set! prog (uar:main src dst prog))
      (set! uar:main #f)
      (uio:list->pp-file dst prog 79)
      (newline)
      (display "Target program has been written into ")
      (display dst)
      (newline)))
  (define (run-ensugar)
    (newline)
    (let* ((src (uio:request-file-name
                  "Source Mixwell program file name"
                  ""
                  "MW"))
           (dst (uio:request-file-name
                  "Target Scheme program file name"
                  src
                  "SCE"))
           (prog (uio:file->list src)))
      (ux:load "xensg")
      (set! prog (uensg:main src dst prog))
      (set! uensg:main #f)
      (uio:list->pp-file dst prog 79)
      (newline)
      (display "Ensugared program has been written into ")
      (display dst)
      (newline)))
  (define (run-form)
    (newline)
    (let* ((src (uio:request-file-name "Source program file name" "" "S"))
           (dst (uio:request-file-name "Target program file name" src "SCF"))
           (prog (uio:file->list src)))
      (uio:list->pp-file dst prog 79)
      (newline)
      (display "Formatted program has been written into ")
      (display dst)
      (newline)))
  (case action
    ((post) (run-post))
    ((cgr) (run-cgr))
    ((ar) (run-ar))
    ((ensugar) (run-ensugar))
    ((form) (run-form))))

