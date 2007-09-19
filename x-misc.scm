;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;  File:     xmisc.scm                                            ;;
;;  Project:  the specializer Unmix                                ;;
;;  Author:   S.A.Romanenko, the Institute for Applied             ;;
;;            Mathematics, Russia's Acedemy of Sciences,           ;;
;;            Moscow.                                              ;;
;;  Created:  December, 1992                                       ;;
;;                                                                 ;;
;;  Contents: Miscellaneous procedures.                            ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Generates new names...

(define ux:gentemp
  (let ((*gensym-counter* 0))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "%%" (number->string *gensym-counter*))))))


;; Applies "f" to each element of "v",
;; and returns a new vector.

(define (vector-map f v)
  (let* ((vl (vector-length v))
         (w  (make-vector vl)))
    (do ((i 0 (+ i 1)))
      ((>= i vl) w)
      (vector-set! w i (f (vector-ref v i))))))

;; Applies "f" to each element of "v"
;; from left to right.

(define (vector-for-each f v)
  (let* ((vl (vector-length v)))
    (do ((i 0 (+ i 1)))
      ((>= i vl) #f)
      (f (vector-ref v i)))))

;; Applies "f" to each element of "lst" and takes
;; "and" of the results obtained.

(define (and-map f lst)
  (let loop ((lst lst))
    (or (null? lst)
        (and (f (car lst))
             (loop (cdr lst))))))

;; Applies "f" to each element of "lst" and takes
;; "or" of the results obtained.

(define (or-map f lst)
  (let loop ((lst lst))
    (and (not (null? lst))
         (or (f (car lst))
             (loop (cdr lst))))))

(define (foldl f u lst)
  (let loop ((u u) (lst lst))
    (if (null? lst)
        u
        (loop (f u (car lst)) (cdr lst)))))

(define (foldl1 f lst)
  (cond
    ((null? lst)
     (error "foldl1: empty argument list:" lst))
    ((null? (cdr lst))
     (car lst))
    (else
      (foldl f (car lst) (cdr lst)))))

;;(define (foldr f u lst)
;;  (let loop ((lst lst))
;;    (if (null? lst)
;;        u
;;        (f (car lst) (loop (cdr lst))))))

(define (foldr f u lst)
  (let loop ((u u) (lst (reverse lst)))
    (if (null? lst)
        u
        (loop (f (car lst) u) (cdr lst)))))

(define (foldr1 f lst)
  (let ((lst (reverse lst)))
    (let loop ((u (car lst)) (lst (cdr lst)))
      (if (null? lst)
          u
          (loop (f (car lst) u) (cdr lst))))))

(define (foldr-2 f u lst1 lst2)
  (when (not (= (length lst1) (length lst2)))
        (error "foldr-2: argument length mismatch:" lst1 lst2))
  (let loop ((u u) (lst1 (reverse lst1)) (lst2 (reverse lst2)))
    (if (null? lst1)
        u
        (loop (f (car lst1) (car lst2) u)
              (cdr lst1)
              (cdr lst2)))))

;; (define (foldr-map f u g lst) (foldr f u (map g lst)))

(define (foldr-map f u g lst)
  (let loop ((u u) (lst (reverse lst)))
    (if (null? lst)
        u
        (loop (f (g (car lst)) u) (cdr lst)))))

;; (define (foldl-map f u g lst) (foldl f u (map g lst)))

(define (foldl-map f u g lst)
  (let loop ((u u) (lst lst))
    (if (null? lst)
        u
        (loop (f u (g (car lst))) (cdr lst)))))

;; (define (memq-map x f lst) (or-map (lambda (a) (eq? x (f a))) lst))

(define (memq-map x f lst)
  (let loop ((lst lst))
    (and (not (null? lst))
         (or (eq? x (f (car lst)))
             (loop (cdr lst))))))

(define (append-map f lst)
  (let loop ((result '()) (lst (reverse lst)))
    (if (null? lst)
        result
        (loop (append (f (car lst)) result) (cdr lst)))))

(define (append!-map f lst)
  (let loop ((result '()) (lst (reverse lst)))
    (if (null? lst)
        result
        (loop (append! (f (car lst)) result) (cdr lst)))))

(define (filter p lst)
  (let loop ((result '()) (lst lst))
    (cond
      ((null? lst)
       (reverse result))
      ((p (car lst))
       (loop (cons (car lst) result) (cdr lst)))
      (else
        (loop result (cdr lst))))))

(define (remove-if p lst)
  (let loop ((result '()) (lst lst))
    (cond
      ((null? lst)
       (reverse result))
      ((p (car lst))
       (loop result (cdr lst)))
      (else
        (loop (cons (car lst) result) (cdr lst))))))

(define (security-filter p lst)
  (let loop ((result '()) (lst lst))
    (cond
      ((null? lst)
       (reverse result))
      ((p (car lst))
       (loop (cons (car lst) result) (cdr lst)))
      ((and-map p (cdr lst))
       (append! (reverse result) (cdr lst)))
      (else
        (error "security-filter: more than one element to delete")))))

(define (find-if p lst)
  (let loop ((lst lst))
    (cond
      ((null? lst) #f)
      ((p (car lst))
       (car lst))
      (else
        (loop (cdr lst))))))

(define (member-if p lst)
  (let loop ((lst lst))
    (cond
      ((null? lst)
       #f)
      ((p (car lst))
       lst)
      (else
        (loop (cdr lst))))))

(define (list-index x lst)
  (let loop ((n 0) (rest lst))
    (cond
      ((null? rest)
       (error "Item not found in list" x lst))
      ((eq? x (car rest))
       n)
      (else
        (loop (+ 1 n) (cdr rest))))))

;; Returns #t iff "lst" contains two occurences
;; of the same symbol.

(define (duplicate-symbols? lst)
  (and (not (null? lst))
       (or (memq (car lst) (cdr lst))
           (duplicate-symbols? (cdr lst)))))
