;;;;;; bidi-edge.rkt - Bidirectional Edge module.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 3 Sep 2012

#lang typed/racket/no-check

(require
 "grammar.rkt"
 "lexer.rkt"
 "utils.rkt")

(provide (all-defined-out))

(struct edge
  (start end lhs left found right assoc prec vars code src)
  #:transparent)

(define (edge-list? x)
  (and (list? x)
       (andmap edge? x)))

(define-match-expander edge-complete
  (syntax-rules ()
    [(edge-complete lhs found)
     (edge _ _ lhs '() found '() _ _ _ _ _)]))

(define (edge-complete? E)
  (and (null? (edge-left E)) (null? (edge-right E))))

(define (edge-incomplete? E)
  (not (edge-complete? E)))

(define (edge->string E)
  (match E
    [(edge start end lhs left found right assoc prec vars code src)
     (format "[~a,~a, ~a -> ~a . ~a . ~a vars=~a src=~a]"
             start end lhs (edge->string left) (edge->string found)
             (edge->string right) vars src)]
    [(? null? E) ""]
    [(? list? E) (format "~a" (map edge->string E))]
    [else (format "~a" E)]))

(: edge->ast
   (case-> [edge -> (Listof Sexp)]
           [edge Boolean -> (Listof Sexp)]))
(define (edge->ast E [terminals? #f])
  (define (as? obj)
    (or (edge? obj)
        (and (pair? obj)
             (or terminals?
                 (and (rule-lhs? (car obj))
                      (not (string? (car obj))))))))
  (if (edge? E)
      `(,(car (edge->ast (edge-lhs E) #f))
        ,@(map (λ (e)
                 (match e
                   [(? edge? e) (edge->ast e terminals?)]
                   [(? sexp? e) (car (edge->ast e terminals?))]
                   [(cons (? sexp? e1) (? token? e2))
                    (car (edge->ast (cons e1 (token-value e2)) terminals?))]
                   [_ (error "edge->ast unexpected:" e)]))
               (if (list1? (edge-found E))
                   (edge-found E)
                   (filter as? (edge-found E)))))
      (if (and (pair? E)
               (string? (car E)))
          (list (car E))
          (list E))))

(: parse-type (edge -> SExpr))
(define (parse-type E)
  (match (edge-found E)
    [(list (cons '*Name (? token? t)))
     (string->symbol (token-value t))]
    [_ (let* ([code (edge-code E)]
              [proc (cdr code)]
              [formals (car code)])
         (let ([ret (proc (map (λ (f)
                                 (lookup (edge-vars E) f))
                               formals))])
           (if (sexpr? ret)
               ret
               (error "expected SExpr; got:" ret))))]))

(: lookup
   (All (T)
        (case-> [(HashTable LHS Any) LHS -> Any]
                [(HashTable LHS Any) LHS (-> T) -> Any])))
(define (lookup vars A [failure-result (λ () #f)])
  (cond [(symbol? A)
         (hash-ref vars A (λ ()
                            (let ([B (symbol->string A)])
                              (lookup vars B failure-result))))]
        [(string? A)
         (match (hash-ref vars A (λ () #f))
           [(list (cons (? string? s) _))
            (string->symbol s)]
           [(list (cons (? edge? e) 'Type))
            (parse-type e)]
           [(list (cons (? edge? e) _))
            (unparse e)]
           [else (failure-result)])]
        [(and (rule-lhs? A) (list? A))
         (map (λ (a)
                (lookup vars a (λ () a)))
              A)]
        [else (failure-result)]))

(: unparse
   (case-> [(U Edge-Term Term) -> String]
           [(U Edge-Term Term) String -> String]))
(define (unparse E [sep ""])
  (cond [(and (edge? E) edge-complete? E)
         (string-join (map (λ (F)
                             (unparse F sep))
                           (edge-found E)) sep)]
        [(and (pair? E) (token? (cdr E))) (token-value (cdr E))]
        [(string? E) E]
        [(rule-lhs? E) (format "~a" E)]
        [else (error "unparse unexpected:" E)]))

(: leaf-count (Any -> Natural))
(define (leaf-count node)
  (match node
    ['() 0]
    [(list (? symbol?) (? string?)) 1]
    [(? list? node)
     (foldl + 0 (map leaf-count (cdr node)))]
    [(? pair?) 1]))

(: edge-leaf-count (Any -> Natural))
(define (edge-leaf-count E)
  (if (and (edge? E)
           (edge-complete? E))
      (leaf-count (edge->ast E))
      0))

(: get-last-token (edge -> (Option token)))
(define (get-last-token e)
  (and (not (null? (edge-found e)))
       (match (last (edge-found e))
         [(cons _ (? token? t)) t]
         [(? token? t) t]
         [(? edge? f) (get-last-token f)])))
