;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;  File:     xio.scm                                              ;;
;;  Project:  the specializer Unmix                                ;;
;;  Author:   S.A.Romanenko, the Institute for Applied             ;;
;;            Mathematics, the USSR Acedemy of Sciences,           ;;
;;            Moscow.                                              ;;
;;  Created:  6 December 1989                                      ;;
;;  Revised:  28 March 1990                                        ;;
;;            27 July 1990                                         ;;
;;            October 1993                                         ;;
;;                                                                 ;;
;;  Contents: The input/output functions, menus etc. ...           ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Clears the screen.

;(define (uio:clear-screen)
;  (do ((i 0 (+ 1 i))) ((>= i *ux:screen-height*) #f) (newline)))

(define (uio:clear-screen)
   (newline))

(define (uio:spaces k)
  (do ((k k (- k 1))) ((<= k 0) #f) (display " ")))

;; Centers and displays a message.

(define (uio:center msg)
  (let ((lm  (quotient (- *ux:screen-width* 1 (string-length msg)) 2) ))
    (uio:spaces lm)
    (display msg)))

(define (uio:display-menu msg* column items prompt replies)

  (define title
   '())

;  '("UNMIX 3.0 (November 1993) for MacGambit Scheme"
;   "   Sergei A.Romanenko, Keldysh Institute of Applied Mathematics,"
;   "   Russian Academy of Sciences, Miusskaya Sq.4, 125047, Moscow, Russia"
;            ))

  (define *line* 1)

  (for-each (lambda (item) (display item) (newline)
                           (set! *line* (+ 1 *line*)))
            title)
  (newline) (newline) (set! *line* (+ 2 *line*))
  (for-each (lambda (item) (uio:center item) (newline)
                           (set! *line* (+ 1 *line*)))
            msg*)
  (newline) (set! *line* (+ 1 *line*))
  (do ((items items (cdr items)))
      ((null? items))
      (uio:spaces column)
      (display (car items))(newline)(set! *line* (+ 1 *line*)))
  (do ((*line* *line* (+ 1 *line*)))
    ((>= *line* *ux:screen-height*))
    (newline))
  (uio:query prompt replies))

;;
;; Reads a character from the keyboard.
;; The character must be on the list lst.
;;

(define (uio:query msg lst)
  (do () ((not (char-ready?))) (read-char))
  (let loop ()
    (if (not (null? msg))
        (begin (display msg) (display ": ")))
    (let ((item (read)))
      (if (member item lst)
          item
          (begin (display " ???") (newline) (loop))))))

;;
;; Waits for a key to be pressed.
;;

(define (uio:pause)
  (newline) (display "Press Enter to continue...") (newline)
  (do () ((not (char-ready?))) (read-char))
  (uio:read-line))

;;
;; Requests a string.
;;

(define (uio:request-string msg)
  (do () ((not (char-ready?))) (read-char))
  (newline) (display msg)
  (uio:read-line))

(define (uio:string-upcase s)
  (let* ((len  (string-length s))
         (new-s  (make-string len)))
    (do ((k 0 (+ 1 k)))
        ((= k len) new-s)
        (string-set! new-s k (char-upcase (string-ref s k))))))

;;
;; Reads characters upto #\newline.
;;

(define (uio:read-line)
  (define (drop-spaces lst)
    (cond ((null? lst) lst)
          ((eqv? (car lst) #\space)
           (drop-spaces (cdr lst)))
          (else lst)))
  (let loop ((lst '()))
    (let ((x (read-char)))
      (case x
        ((#\newline)
         (list->string (drop-spaces (reverse (drop-spaces lst)))))
        (else
          (loop (cons x lst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;                       Input and Output                          ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Reads a Scheme program from the file "src".
;;

(define (uio:file->list file)
  (let ((port  (open-input-file file)))
    (let loop ((acc '()))
      (let ((o (read port)))
        (if (eof-object? o)
            (begin (close-input-port port) (reverse acc))
            (loop (cons o acc)))))))

(define (uio:list->file name lst)
  (let ((p (open-output-file name)))
    (uio:write-list lst p)
    (close-output-port p)))

(define (uio:write-list lst port)
  (for-each
    (lambda (item) (write item port) (newline port) (newline port))
    lst))

;;
;; Pretty prints list "lst" into the file "name".
;;

(define (uio:list->pp-file file lst width)
  (let ((p (open-output-file file)))
    (uio:pp-list lst p)
    (close-output-port p)))

(define (uio:pp-list exp* port)
  (for-each (lambda (exp) (pretty-print exp port) (newline port)) exp*))

;;
;; Requests name.
;;

(define (uio:request-name msg name)
  (let* ((name (uio:cut-off-ext name))
         (str  (uio:request-string (string-append msg " [" name "]:"))))
    (if (string=? str "") name str)))

;;
;; Requests file name.
;;

(define (uio:request-file-name msg name ext)
  (let ((name (uio:cut-off-ext name)))
    (uio:norm-file-name
      name ext
      (uio:request-string (string-append msg " [" name "." ext "]: ")))))

;;
;; Normalizes file name.
;;

(define (uio:norm-file-name name ext name-ext)
  (let ((len (string-length name-ext)))
    (if (not (zero? len))
        (let ((pos (uio:char-pos #\. name-ext)))
          (cond
            ((not pos) (set! name name-ext))
            ((zero? pos) (set! ext (substring name-ext 1 len)))
            (else
              (set! name (substring name-ext 0 pos))
              (set! ext  (substring name-ext (+ pos 1) len)))))))
  (string-append name "." ext))

;;
;; Cuts off file name extension.
;;

(define (uio:cut-off-ext name-ext)
  (let ((pos (uio:char-pos #\. name-ext)))
    (if pos
        (substring name-ext 0 pos)
        name-ext)))

(define (uio:char-pos char str)
  (let ((len  (string-length str)))
    (let loop ((n 0))
      (cond
        ((= n len)  #f)
        ((eqv? char (string-ref str n))  n)
        (else
          (loop (+ n 1)))))))
