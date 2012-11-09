;;;;;; grammar.rkt - Grammar module.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 9 Nov 2012

#lang typed/racket/no-check

(require "utils.rkt")

(provide (all-defined-out))

(define (rule-lhs? x)
  (or (string? x)
      (sexpr? x)))

(define (sexpr? x)
  (or (symbol? x)
      (and (list? x)
           (andmap sexpr? x))))

(define (sexp? x)
  (or (symbol? x)
      (string? x)
      (char? x)
      (boolean? x)
      (number? x)
      (and (or (vector? x)
               (list? x))
           (for/and ([e x])
             (sexp? e)))
      (and (pair? x)
           (sexp? (car x))
           (sexp? (cdr x)))))

(define (sexp-list? x)
  (and (list? x)
       (andmap sexp? x)))

(define (symbol-list? x)
  (and (list? x)
       (andmap symbol? x)))

(define (assoc? x)
  (or (eq? '⊥ x)
      (eq? 'left x)
      (eq? 'right x)
      (eq? 'non x)))

(define (prec? x)
  (or (eq? '⊥ x) (zero? x) (exact-positive-integer? x)))

(struct grammar (start rules name id) #:transparent)

(struct rule (lhs rhs assoc prec vars code src) #:transparent)

(struct half-rule (rhs assoc prec vars code) #:transparent)

(define-type Rule-Spec
  (Pairof LHS (Pairof '::= (Listof (U (Listof Term) half-rule)))))

(define-syntax-rule (define-grammar name rule ...)
  (define name (mk-grammar `(rule ...) 'name)))

(: mk-grammar ((Listof Rule-Spec) Symbol -> grammar))
(define (mk-grammar rules name)
  (define id (interned-gensym 'GID))
  (define (gensym-rule) (cons id (interned-gensym name)))

  (: expand-rule ((Listof Term) -> (Listof (Listof Term))))
  (define (expand-rule ts)
    (if (null? ts)
        (list ts)
        (let ([rst (expand-rule (cdr ts))])
          (match (car ts)
            [(list '? t1)
             (append rst (map (λ (rs)
                                (cons t1 rs))
                              rst))]
            [t1 (map (λ (rs)
                       (cons t1 rs))
                     rst)]))))

  (: format-rule (Rule-Spec -> (Listof rule)))
  (define (format-rule t)
    (let loop ([rls (cddr t)] [rs '()])
      (if (null? rls)
          (reverse rs)
          (if (half-rule? (car rls))
              (match (car rls)
                [(half-rule rhs assoc prec vars code)
                 (let* ([src (gensym-rule)]
                        [r (rule (car t) rhs assoc prec vars code src)])
                   (loop (cdr rls) (cons r rs)))])
              (let ([rst (map (λ (rhs)
                                (let ([lhs (car t)]
                                      [src (gensym-rule)])
                                  (rule lhs rhs '⊥ '⊥ (make-hash) #f src)))
                              (expand-rule (car rls)))])
                (loop (cdr rls) (append rst rs)))))))
                     
  (let* ([rrs (map format-rule rules)]
         [rh (make-hash)])
    (for ([rs rrs])
      (for ([r rs])
        (hash-update!
         rh (rule-lhs r) (λ (ss) (cons r ss)) null)))
    (let ([S (if (null? rrs) 'grammar (rule-lhs (caar rrs)))])
      (grammar S rh name id))))

(: add-lexicon (String grammar -> grammar))
(define (add-lexicon word g)
  (if (category? word word g)
      g
      (let* ([src (cons (grammar-id g) (string->symbol word))]
             [r (rule word (list word) '⊥ '⊥ (make-hash) #f src)]
             [update (λ (rs) (cons r rs))])
        (grammar
         (grammar-start g)
         (hash-update (grammar-rules g) word update null)
         (grammar-name g)
         (grammar-id g)))))

(: grammar-union (grammar grammar -> grammar))
(define (grammar-union g h)
  (let ([rules (make-hash)])
    (hash-for-each
     (grammar-rules g)
     (λ (lhs rs)
       (hash-set! rules lhs rs)))
    (hash-for-each
     (grammar-rules h)
     (λ (lhs rs)
       (let ([update (λ (grs) (append grs rs))])
         (hash-update! rules lhs update null))))
    (let ([S (grammar-start (if (eq? 'grammar (grammar-start g)) h g))])
      (grammar S rules 'U (interned-gensym 'U)))))

(: rewrites-for (LHS grammar -> (Listof rule)))
(define (rewrites-for x g)
  
  (: subst (Term rule -> rule))
  (define (subst x r)
    (match r
      [(rule lhs rhs assoc prec vars code src)
       (let loop ([rhs rhs] [new-rhs '()])
         (if (null? rhs)
             (let ([vars (hash-copy vars)])
               (hash-set! vars lhs x)
               (rule lhs (reverse new-rhs) assoc prec vars code src))
             (let ([t (car rhs)])
               (cond [(equal? t lhs)
                      (loop (cdr rhs) (cons x new-rhs))]
                     [(and (pair? t) (string? (car t)) (equal? (cdr t) lhs))
                      (let ([p (cons (car t) x)])
                        (loop (cdr rhs) (cons p new-rhs)))]
                     [else (loop (cdr rhs) (cons (car rhs) new-rhs))]))))]))
  
  (: lookup (LHS (HashTable LHS (Listof rule)) -> (Listof rule)))
  (define (lookup x rules)
    (let loop ([rvs (expand-rules rules)] [rs '()])
      (if (null? rvs)
          (reverse rs)
          (let ([r (car rvs)])
            (cond [(equal? (rule-lhs r) x)
                   (loop (cdr rvs) (cons r rs))]
                  [(parameterized-rule? r)
                   (loop (cdr rvs) (cons (subst x r) rs))]
                  [else (loop (cdr rvs) rs)])))))
  
  (if (string? x)
      (let ([src (cons (grammar-id g) 'str)])
        (list (rule x (list x) '⊥ '⊥ (make-hash) #f src)))
      (let ([rs (lookup x (grammar-rules g))])
        (if (not (null? rs))
            rs
            (if (rule-rec? x)
                (let ([rhs (list (rule-base x) x)]
                      [src (cons (grammar-id g) 'rec)])
                  (list (rule x rhs '⊥ '⊥ (make-hash) #f src)))
                '())))))

(: category (String grammar -> (Listof LHS)))
(define (category word g)
  (filter (λ (cat)
            (category? word cat g))
          (hash-keys (grammar-rules g))))

(: category? (String LHS grammar -> Boolean))
(define (category? word category g)
  (ormap (λ (r)
           (and (list1? (rule-rhs r))
                (rule-match? (car (rule-rhs r)) word)))
         (hash-ref (grammar-rules g) word null)))

(: rule-match? (Term Term -> Boolean))
(define (rule-match? x y)
  (or (and (regexp? x) (string? y) 
           (not (not (regexp-match x y))))
      (and (procedure? x) (string? y) 
           (not (not (x y))))
      (equal? x y)))

(: get-lexical-term ((U rule Rule-Spec) -> (Option Term)))
(define (get-lexical-term r)
  (if (rule? r)
      (match (rule-rhs r)
        [(list t) t]
        [_ #f])
      (match (cddr r)
        [(list (half-rule (list t) _ _ _ _)) t]
        [(list (list t)) t]
        [_ #f])))

(: lexical-rule? ((U rule Rule-Spec) -> Boolean))
(define (lexical-rule? r)
  (let ([t (get-lexical-term r)])
    (or (string? t)
        (regexp? t)
        (procedure? t))))

(: grammar-lexicon (grammar -> (Listof rule)))
(define (grammar-lexicon g)
  (filter lexical-rule? (all-rules g)))

(: rule-rec? (LHS -> Boolean))
(define (rule-rec? category)
  (and (symbol? category)
       (let ([lhs (symbol->string category)])
         (char=? #\+ (string-ref lhs (- (string-length lhs) 1))))))

(: rule-base (LHS -> LHS))
(define (rule-base category)
  (if (symbol? category)
      (let ([lhs (symbol->string category)])
        (string->symbol (substring lhs 0 (- (string-length lhs) 1))))
      category))

(: parameterized-rule? (rule -> Boolean))
(define (parameterized-rule? rule)
  (hash-has-key? (rule-vars rule) (rule-lhs rule)))

(: more-specific? (LHS LHS grammar -> Boolean))
(define (more-specific? A B g)
  ;; return B =>* A in g
  (let loop ([rules (rewrites-for B g)])
    (and (not (null? rules))
         (let ([rhs (rule-rhs (car rules))])
           (or (and (list1? rhs)
                    (let ([t (car rhs)])
                      (or (equal? A t)
                          (and (pair? t)
                               (equal? A (cdr t)))
                          (and (rule-lhs? t) (not (string? t))
                               (more-specific? A t g)))))
               (loop (cdr rules)))))))

(: is-derivation? (String grammar -> Boolean))
(define (is-derivation? a g)
  ;; return A => a for some A in g
  (ormap (λ (r)
           (or (and (list1? (rule-rhs r))
                    (rule-match? (car (rule-rhs r)) a))
               (and (string? a)
                    (let loop ([rhs (rule-rhs r)] [a? #f])
                      (if (null? rhs)
                          a?
                          (or (and (rule-match? (car rhs) a)
                                   (loop (cdr rhs) #t))
                              (and (string? (car rhs))
                                   (loop (cdr rhs) a?))))))))
         (expand-rules (grammar-rules g))))

(: fun-def? (rule -> Boolean))
(define (fun-def? rule)
  (not (not (rule-code rule))))

(: expand-rules ((HashTable LHS (Listof rule)) -> (Listof rule)))
(define (expand-rules rules)
  (let loop ([rs (hash-values rules)]
             [ret '()])
    (if (null? rs)
        ret
        (loop (cdr rs) (append ret (car rs))))))

(: all-rules (grammar -> (Listof rule)))
(define (all-rules g)
  (apply append (hash-values (grammar-rules g))))

(define null (λ () '()))
