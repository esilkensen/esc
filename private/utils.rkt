;;;;;; utils.rkt - Utilities module.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 3 Sep 2012

#lang typed/racket/no-check

(require srfi/13) ;; string library

(provide (all-defined-out))

(: interned-gensym (Symbol -> Symbol))
(define (interned-gensym base)
  (string->symbol (symbol->string (gensym base))))

(: filename-base (String -> String))
(define (filename-base filename)
  (substring filename 0 (string-index-right filename #\.)))

(: filename-dir (String -> String))
(define (filename-dir filename)
  (let ([cp (path->complete-path (string->path filename))])
    (let-values ([(base name must-be-dir?) (split-path cp)])
      (if (path? base)
          (path->string base)
          (error filename)))))

(: mapcar (All (T) ((Listof (Pairof T Any)) -> (Listof T))))
(define (mapcar lst)
  (let loop ([lst lst] [ret '()])
    (if (null? lst)
        (reverse ret)
        (loop (cdr lst) (cons (caar lst) ret)))))

(: flatten ((Listof Any) -> (Listof Any)))
(define (flatten lst)
  (if (null? lst)
      lst
      (append (if (list? (car lst))
                  (flatten (car lst))
                  (list (car lst)))
              (flatten (cdr lst)))))

(: list1? (All (T) (T -> Boolean)))
(define (list1? obj) (listn? 1 obj))

(: list2? (All (T) (T -> Boolean)))
(define (list2? obj) (listn? 2 obj))

(: list3? (All (T) (T -> Boolean)))
(define (list3? obj) (listn? 3 obj))

(: list4? (All (T) (T -> Boolean)))
(define (list4? obj) (listn? 4 obj))

(: listn? (All (T) (Natural T -> Boolean)))
(define (listn? n obj) (and (list? obj) (= n (length obj))))

(: print-hash (All (S T) ((HashTable S T) -> Void)))
(define (print-hash h)
  (hash-for-each h
    (lambda (k v)
      (printf "      ~s => " k)
      (pretty-print v))))
