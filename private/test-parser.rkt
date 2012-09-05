;;;;;; test-parser.rkt - Test parse-file.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 10 Nov 2011

#lang racket

(require "es.rkt")
(require "utils.rkt")

(provide (all-defined-out)
         parse-file/island parse-file/td-earley parse-file/bu-earley)

(define (read-file filename)
  (with-input-from-file filename (lambda () (read))))

(define (test-file filename [parse-file parse-file/island] [time? #f])
  (equal? (parse-file filename #f time?)
          (read-file (string-append (filename-base filename) ".ast"))))

(define (is-test-file? filename)
  (let ([ext (filename-extension (string->path filename))])
    (and (string=? "es" (if ext (bytes->string/utf-8 ext) ""))
         (let ([base (filename-base filename)])
           (file-exists? (string->path (format "~a.ast" base)))))))

(define (test-dir dir [parse-file parse-file/island] [time? #f])
  (let loop ([fs (directory-list dir)] [fc 0] [pc 0])
    (if (null? fs)
        (printf "---------~n~a/~a TESTS PASSED.~n" pc (+ fc pc))
        (let ([f (car fs)])
          (if (is-test-file? (format "~a/~a" dir f))
              (let ([filename (format "~a/~a" dir (path->string f))])
                (printf "~a: " f)
                (let ([res (test-file filename parse-file time?)])
                  (if res
                      (unless time? (printf "ok.~n"))
                      (printf "  ~a test failed!~n" f))
                  (loop (cdr fs) (if res fc (+ fc 1)) (if res (+ pc 1) pc))))
              (loop (cdr fs) fc pc))))))
