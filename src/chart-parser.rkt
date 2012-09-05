;;;;;; chart-parser.rkt - Chart Parser module.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 3 Sep 2012

#lang typed/racket/no-check

(require
 "bidi-edge.rkt"
 "grammar.rkt"
 "lexer.rkt"
 "utils.rkt")

(provide (all-defined-out))

(define-type Chart
  (Pairof (Vectorof (Listof edge))
          (Vectorof (Listof edge))))

(define (binding? x)
  (and (pair? x)
       (string? (car x))))

(struct: parser-state
  ([chart : Chart]
   [depend : (HashTable edge (Listof edge))]
   [spec : (HashTable Symbol (Listof LHS))]
   [counts : (HashTable LHS Integer)]
   [stats : (HashTable LHS Integer)]
   [agenda : (Listof edge)]
   [aux : (Listof (Listof edge))]))

(struct: parser-ops
  ([init-state : (Option parser-state)]
   [ret-state? : Boolean]
   [left? : Boolean]
   [top-down? : Boolean]
   [spec? : Boolean]
   [trace? : Boolean]
   [report? : Boolean]))

(struct: agenda-state
  ([edges : (Listof edge)]
   [aux : (Listof (Listof edge))]
   [hook : (-> Any)]))

(define *match-trace?* #f)

(: chart-parse
   ((Vectorof token) grammar (edge (Listof edge) -> (Listof edge)) LHS
    parser-ops -> (U (Vectorof (Listof edge)) parser-state)))
(define (chart-parse tokens grammar enqueue S ops)
  (define init-state (parser-ops-init-state ops))
  (define top-down? (parser-ops-top-down? ops))
  (define trace? (parser-ops-trace? ops))
  (define chart
    (if init-state (parser-state-chart init-state)
        (initial-chart tokens (parser-ops-top-down? ops) grammar)))
  (define chart-depend
    (if init-state (parser-state-depend init-state) (make-hash)))
  (define param-spec
    (if init-state (parser-state-spec init-state) (make-hash)))
  (define counts
    (if init-state (parser-state-counts init-state) (make-hash)))
  (define stats
    (if init-state (parser-state-stats init-state) (make-hash)))
  (define agenda
    (let* ([edges (if init-state (parser-state-agenda init-state)
                      (initial-agenda tokens grammar S top-down?))]
           [aux (if init-state (parser-state-aux init-state) '())]
           [hook (λ ()
                   (when (parser-ops-report? ops)
                     (report stats))
                   (if (parser-ops-ret-state? ops)
                       (parser-state chart chart-depend param-spec
                                     counts stats edges aux)
                       (cdr chart)))])
      (list (agenda-state edges aux hook))))
  
  (: process-edge! (edge -> Void))
  (define (process-edge! E)
    (unless (in-chart? E)
      (add-to-chart! E 'process-edge!)
      (if (edge-incomplete? E)
          (begin (when (close-rec? E)
                   (let ([a '()]
                         [b '()]
                         [c (mk-close-rec E)])
                     (set! agenda (cons (agenda-state a b c) agenda))))
                 (forward-fundamental-rule E)
                 (when top-down? (top-down-rule E)))
          (begin (backward-fundamental-rule E)
                 (unless top-down? (bottom-up-rule E))))))

  (: forward-fundamental-rule (edge -> Void))
  (define (forward-fundamental-rule E)
    ;; E : [i,j, A -> alpha B . beta . D delta]
    ;; for all [h,i, B -> . gamma .] or [j,k, D -> . gamma .], add
    ;; [h,j, A -> alpha . B beta . D delta] or
    ;; [i,k, A -> alpha B . beta D . delta] to the agenda
    (: mk-compl? (Integer Term (edge -> Integer) -> (edge -> Boolean)))
    (define (mk-compl? n B/D edge-pt)
      (λ (e)
        (and (edge-complete? e)
             (or (match? B/D (edge-lhs e))
                 (let ([vars* (hash-copy (edge-vars E))])
                   (match! B/D (edge-lhs e) vars* (edge-src E) e))
                 (let ([vars* (hash-copy (edge-vars E))])
                   (match* B/D e vars* (edge-src E))))
             (= n (edge-pt e)))))
    (match E
      [(edge i j A alpha beta delta assoc prec vars code src)
       (let ([ffr (λ (n B/D edge-pt pt gen-dir)
                    (let ([compl? (mk-compl? n B/D edge-pt)])
                      (for ([e (chart-filter compl? chart n pt)])
                        (let-values ([(i j l f r) (gen-dir e)])
                          (let ([vars* (hash-copy vars)])
                            (when (or
                                   (match? B/D (edge-lhs e))
                                   (let ([vars** (hash-copy vars*)])
                                     (and (match! B/D (edge-lhs e) vars** src e)
                                          (begin (set! vars* vars**) #t)))
                                   (let ([vars** (hash-copy vars*)])
                                     (and (match* B/D e vars** src)
                                          (begin (set! vars* vars**) #t))))
                              (set! A (param-subst A l f r vars*))
                              (let ([b/d (edge i j A l f r
                                               assoc prec vars* code src)])
                                (when (edge? B/D)
                                  (add-to-depend! B/D b/d 'ffr))
                                (add-to-agenda! b/d 'ffr))))))))]
             [gen-left (λ (e)
                         (values (edge-start e) j (drop-right alpha 1)
                                 (append (compl-edge e) beta) delta))]
             [gen-right (λ (e)
                          (values i (edge-end e) alpha
                                  (append beta (compl-edge e)) (cdr delta)))])
         (unless (or (not (parser-ops-left? ops)) (null? alpha))
           (ffr i (last alpha) edge-end 'end gen-left))
         (unless (null? delta)
           (ffr j (first delta) edge-start 'start gen-right)))]))

  (: top-down-rule (edge -> Void))
  (define (top-down-rule E)
    ;; E : [i,j, A -> alpha B . beta . D delta]
    ;; for all B ::= gamma; or D ::= gamma; in the grammar, add
    ;; [i,i, B -> . . gamma] or [j,j, D -> . . gamma] to the agenda
    (: pred-td (Integer Term (HashTable LHS Any) -> Void))
    (define (pred-td n B/D vars)
      (let ([lhs (if (binding? B/D) (cdr B/D)
                     (if (rule-lhs? B/D) B/D #f))])
        (when (and (rule-lhs? lhs) (not (string? lhs)))
          (let ([param? (eq? #f (hash-ref vars lhs (λ () lhs)))])
            (for ([r (if param? (all-rules grammar)
                         (rewrites-for lhs grammar))])
              (match r
                [(rule lhs rhs assoc prec vars code src)
                 (let* ([vs (hash-copy vars)]
                        [e (edge n n lhs '() '() rhs assoc prec vs code src)])
                   (add-to-depend! E e 'pred-td) ;; is this right?
                   (add-to-agenda! e 'pred-td))]))))))
    (match-let ([(edge i j A alpha beta delta assoc prec vars code src) E])
      (unless (or (not (parser-ops-left? ops)) (null? alpha))
        (pred-td i (last alpha) vars))
      (unless (null? delta)
        (pred-td j (first delta) vars))))

  (: backward-fundamental-rule (edge -> Void))
  (define (backward-fundamental-rule E)
    ;; E : [j,k, B -> . gamma .] -> Void
    ;; for all [k,l, A -> alpha B . beta . delta] or
    ;; [i,j, A -> alpha . beta . B delta], add
    ;; [j,l, A -> alpha . B beta . delta] or
    ;; [i,k, A -> alpha . beta B . delta] to the agenda
    (: mk-compl? (Integer Term (edge -> Integer) (edge -> (Listof Term))
                          ((Listof Term) -> Term) -> (edge -> Boolean)))
    (define (mk-compl? n B/D edge-pt edge-dir sel)
      (λ (e)
        (and (not (null? (edge-dir e)))
             (or (match? (sel (edge-dir e)) B/D)
                 (let ([vars* (hash-copy (edge-vars e))])
                   (match! (sel (edge-dir e)) B/D vars* (edge-src e) E))
                 (let ([vars* (hash-copy (edge-vars e))])
                   (match* (sel (edge-dir e)) E vars* (edge-src e))))
             (= n (edge-pt e)))))
    (: mk-left? (Integer Term -> (edge -> Boolean)))
    (define (mk-left? k B) (mk-compl? k B edge-start edge-left last))
    (: mk-right? (Integer Term -> (edge -> Boolean)))
    (define (mk-right? j B) (mk-compl? j B edge-end edge-right first))
    (: bfr
       (Integer Term (Integer Term -> (edge -> Boolean)) (U 'start 'end)
                (Integer Integer (Listof Term) (Listof Edge-Term) (Listof Term)
                         -> (Values Integer Integer (Listof Term)
                                    (Listof Edge-Term) (Listof Term))) -> Void))
    (define (bfr n B compl? pt gen-dir)
      (for ([e (chart-filter (compl? n B) chart n pt)])
        (match e
          [(edge i j A alpha beta delta assoc prec vars code src)
           (let-values ([(i j l f r) (gen-dir i j alpha beta delta)])
             (let ([sel (if (eq? 'end pt) first last)]
                   [edge-dir (if (eq? 'end pt) edge-right edge-left)])
               (when (or (match? (sel (edge-dir e)) B)
                         (let ([vars* (hash-copy vars)])
                           (and (match! (sel (edge-dir e)) B vars* src E)
                                (begin (set! vars vars*) #t)))
                         (let ([vars* (hash-copy vars)])
                           (and (match* (sel (edge-dir e)) E vars* src)
                                (begin (set! vars vars*) #t))))
                 (set! A (param-subst A l f r vars))
                 (let ([b (edge i j A l f r assoc prec vars code src)])
                   (for ([f beta])
                     (when (edge? f)
                       (add-to-depend! f b 'bfr)))
                   (add-to-depend! E b 'bfr)
                   (add-to-agenda! b 'bfr)))))])))
    (match E
      [(edge j k B '() gamma '() _ _ _ _ _)
       (let ([gen-left (λ (k l a b d)
                         (values j l (drop-right a 1)
                                 (append (compl-edge E) b) d))]
             [gen-right (λ (i j a b d)
                          (values i k a (append b (compl-edge E)) (cdr d)))])
         (when (parser-ops-left? ops)
           (bfr k B mk-left? 'start gen-left))
         (bfr j B mk-right? 'end gen-right))]))
  
  (: bottom-up-rule (edge -> Void))
  (define (bottom-up-rule E)
    ;; E : [i,j, B -> . gamma .] -> Void
    ;; for all A ::= alpha B beta; in the grammar, add
    ;; [i,j, A -> alpha . B . beta] to the agenda
    (match E [(edge i j B '() gamma '() _ _ _ _ _)
      (let ([expand? (and (not (null? gamma))
                          (or (not (parser-ops-left? ops))
                              (should-expand? B grammar)))])
        (for ([r (expand-rules (grammar-rules grammar))])
          (let ([assoc (rule-assoc r)]
                [prec (rule-prec r)]
                [vars0 (rule-vars r)]
                [code (rule-code r)]
                [src (rule-src r)]
                [left-aux '()])
            (let loop ([alpha '()] [beta (rule-rhs r)])
              (unless (or (null? beta)
                          (not (or (parser-ops-left? ops) (null? alpha)))
                          (> (length alpha) i))
                (let ([b (car beta)]
                      [vars (hash-copy vars0)])
                  (when (and (or (match? b B)
                                 (let ([vars* (hash-copy vars)])
                                   (and (match! b B vars* src E)
                                        (set! vars vars*) #t))
                                 (let ([vars* (hash-copy vars)])
                                   (and (match* b E vars* src)
                                        (set! vars vars*) #t)))
                             expand?)
                    (let ([A (rule-lhs r)]
                          [C (compl-edge E)]
                          [D (cdr beta)])
                      (set! A (param-subst A alpha C D vars))
                      (let ([e (edge i j A alpha C D assoc prec vars code src)])
                        (cond
                         [(null? alpha)
                          (add-to-depend! E e 'pred-bu)
                          (add-to-agenda! e 'pred-bu)]
                         [(and (rule-lhs? (last alpha))
                               (not (string? (last alpha))))
                          (add-to-depend! E e 'pred-bu)
                          (set! left-aux (cons e left-aux))]
                         [else
                          (let ([a (last alpha)])
                            (if (list1?
                                 (chart-filter 
                                  (λ (f)
                                    (and ;;(= (- i 1) (edge-start f))
                                         (= i (edge-end f))
                                         (edge-complete? f)
                                         (equal? a (edge-lhs f))))
                                  chart
                                  i 'end))
                                (begin 
                                  (add-to-depend! E e 'pred-bu)
                                  (add-to-agenda! e 'pred-bu))
                                (begin
                                  (add-to-depend! E e 'pred-bu)
                                  (set! left-aux (cons e left-aux)))))]))))
                  (loop (append alpha (list b)) (cdr beta)))))
            (add-to-aux! left-aux 'pred-bu)))
        (when (string? B)
          (match gamma
            [(list (? token? b))
             (for ([A (category B grammar)])
               (let* ([src (cons (grammar-id grammar) 'rec)]
                      [vars (make-hash)]
                      [found (list (cons B b))]
                      [e (edge i j A '() found '() '⊥ '⊥ vars #f src)])
                 (add-to-depend! E e 'scan-bu)
                 (add-to-agenda! e 'scan-bu)))]
            [_ (error "expected token:" gamma)])))]))

  (: close-rec? (edge -> Boolean))
  (define (close-rec? E)
    (match E
      [(edge i j A+ '() gamma (list A+) _ _ _ _ _)
       (rule-rec? A+)]
      [else #f]))

  (: mk-close-rec (edge -> (-> Void)))
  (define (mk-close-rec E)
    (match E [(edge i j A+ '() gamma (list A+) assoc prec vars code src)
      (let* ([A (rule-base A+)] [c (hash-ref counts A)])
        (λ ()
          (when (= c (hash-ref counts A))
            (let ([e (edge i j A+ '() gamma '() assoc prec vars code src)])
              (add-to-agenda! e 'close-rec)))))]))
  
  (: in-chart? (edge -> Boolean))
  (define (in-chart? E)
    (or (not (not (member E (vector-ref (car chart) (edge-start E)))))
        (not (not (member E (vector-ref (cdr chart) (edge-end E)))))))

  (: add-to-chart! (edge Symbol -> Void))
  (define (add-to-chart! E caller)
    (: add! (edge Integer (Vectorof (Listof edge)) -> Void))
    (define (add! E j chart)
      (let ([lst (vector-ref chart j)])
        (unless (member E lst)
          ;;(hash-update! stats caller (λ (c) (+ c 1)) 0)
          ;;(hash-update! stats (edge-lhs E) (λ (c) (+ c 1)) 0)
          (vector-set! chart j (cons E lst)))))
    (unless (edge? E)
      (error (format "~a tried to add a non-edge to the chart:~n~a"
                     caller E)))
    (when (not (null? (edge-found E)))
      (let ([update (λ (c) (+ c 1))]
            [zero (λ () 0)])
        (hash-update! counts (edge-lhs E) update zero)))
    (when trace? (log-trace (format "~a.chart" caller) E))
    (add! E (edge-start E) (car chart))
    (add! E (edge-end E) (cdr chart)))

  (: add-to-agenda! (edge Symbol -> Void))
  (define (add-to-agenda! E caller)
    (unless (edge? E)
      (error (format "~a tried to add a non-edge to the agenda:~n~a"
                     caller E)))
    (unless (member E (agenda-state-edges (car agenda)))
      (when (and #t trace?) (log-trace (format "~a.agenda" caller) E))
      (let ([update (λ (c) (+ c 1))]
            [zero (λ () 0)])
        (hash-update! stats caller update zero)
        (hash-update! stats (edge-lhs E) update zero))
      (match (car agenda)
        [(agenda-state es aux hook)
         (let ([fs (enqueue E es)])
           (set! agenda (cons (agenda-state fs aux hook) (cdr agenda))))])))

  (: add-to-aux! ((Listof edge) Symbol -> Void))
  (define (add-to-aux! ES caller)
    (match-let ([(agenda-state edges aux hook) (car agenda)])
      (unless (null? ES)
        (when (and #f trace?)
          (printf "~a.aux:~n" caller)
          (for ([e ES]) (printf "  ~a~n" (edge->string e))))
        (let ([as (agenda-state edges (cons ES aux) hook)])
          (set! agenda (cons as (cdr agenda)))))))

  (: add-to-depend! (edge edge Symbol -> Void))
  (define (add-to-depend! E F caller)
    ;; set F depends on E
    (let ([e (equal-hash-code E)])
      (unless (eq? e (equal-hash-code F))
        (let ([update (λ (l) (cons F l))]
              [null (λ () '())])
          (hash-update! chart-depend E update null)))))

  (: remove-from-chart! (edge Symbol -> Void))
  (define (remove-from-chart! E caller)
    (: remove! (edge Integer (Vectorof (Listof edge)) -> Void))
    (define (remove! E j chart)
      (let ([lst (vector-ref chart j)])
        (vector-set! chart j (remove E lst))))
    (when trace? (log-trace (format "~a.remove!" caller) E))
    (when (in-chart? E)
      (let ([null (λ () '())])
        (remove! E (edge-start E) (car chart))
        (remove! E (edge-end E) (cdr chart))
        (for-each (λ (e)
                    (remove-from-chart! e caller))
                  (hash-ref chart-depend E null)))))

  (: match? (Term Term -> Boolean))
  (define (match? A B)
    (when (and *match-trace?* trace?)
      (printf "match?~n  A = ~a~n  B = ~a~n"
              (pretty-format A) (pretty-format B)))
    (cond [(string? A)
           (and (string? B)
                (string=? A B))]
          [(regexp? A)
           (and (string? B)
                (not (not (regexp-match A B))))]
          [(procedure? A)
           (and (string? B)
                (not (not (A B))))]
          [(rule-lhs? A)
           (equal? A B)]
          [else #f]))
  
  (: match!
     (Term Term (HashTable LHS Any) (Pairof Symbol Symbol) edge -> Boolean))
  (define (match! A B vars src E)
    ;; A is parameterized variable; vars and src belong to A's edge;
    ;; update vars if necessary
    (when (and *match-trace?* trace?
               (or (rule-lhs? A) (binding? A)) (rule-lhs? B))
          (printf "match!~n  A = ~a~n  B = ~a~n"
                  (pretty-format A) (pretty-format B)))
    (if (and (binding? A) (rule-lhs? B) (not (string? B))
             (string? (car A)) (rule-lhs? (cdr A)) (not (string? (cdr A)))
             (match! (cdr A) B vars src E)
             (edge-complete? E))
        (let ([val (or (and (parsed-lexical? E) (unparse E)) E)]
              [type (hash-ref vars (car A))])
          (if (pair? type)
            (begin
              (hash-set! vars (car A) (list (cons val (cdr type)))) #t)
            (error "expected pair:" type)))
        (and (rule-lhs? A) (rule-lhs? B)
             (not (string? A)) (not (string? B))
             (or (match-param! A B vars src)
                 (match? A B)
                 (and (list? A) (list? B)
                      (= (length A) (length B))
                      (andmap (λ (a b)
                                (match! a b vars src E))
                              A B))))))

  (: match-param!
     (Term Term (HashTable LHS Any) (Pairof Symbol Symbol) -> Boolean))
  (define (match-param! A B vars src)
    (: already-more-specific? (LHS -> Boolean))
    (define (already-more-specific? t)
      (and (rule-lhs? B) (not (string? B)) (more-specific? t B grammar)))
    (when (and *match-trace?* trace? (rule-lhs? A) (rule-lhs? B)
               (not (string? A)) (not (string? B)))
          (printf "match-param!~n  A = ~a~n  B = ~a~n"
                  (pretty-format A) (pretty-format B)))
    (and (rule-lhs? A) (rule-lhs? B)
         (not (string? A)) (not (string? B))
         (hash-has-key? vars A)
         (let ([a (hash-ref vars A)])
           (or (and (rule-lhs? a) (not (string? a))
                    (match? a B))
               (and (eq? a #f)
                    (let ([k (cdr src)]
                          [v (λ (l) l)]
                          [null (λ () '())])
                      (hash-update! param-spec k v null)
                      (let ([ts (hash-ref param-spec k)])
                        ;; param-spec optimization:
                        ;; don't allow any subs when B is
                        ;; less specific than any prev sub
                        (and (not (ormap already-more-specific? ts))
                             (begin (hash-set! vars A B)
                                    (hash-update!
                                     param-spec k
                                     (λ (l)
                                       (cons B l)))
                                    #t)))))))))

  (: match*
     (Term edge (HashTable LHS Any) (Pairof Symbol Symbol) -> Boolean))
  (define (match* A B vars src)
    ;; A is a pair of String x Symbol for a bound variable e.g.
    ;; x:Id = ("x" . Id) and B is an edge to match against the cdr;
    ;; if successful, bind the variable to the parse of B e.g.
    ;; B = [i,j, Id -> . gamma .] and "x" : (gamma . _) in vars.
    (: get-val (edge -> (U edge String)))
    (define (get-val B)
      (or (and (parsed-lexical? B) (unparse B)) B))
    (when (and *match-trace?* trace? (binding? A) (edge? B))
          (printf "match*~n  A = ~a~n  B = ~a~n"
                  (pretty-format A) (edge->string B)))
    (and (binding? A) (edge? B)
         (match? (cdr A) (edge-lhs B))
         (hash-has-key? vars (car A))
         (let ([val-type (hash-ref vars (car A))])
           (and (pair? val-type)
                (not (car val-type))
                (let ([val (get-val B)]
                      [type (cdr val-type)])
                  (hash-set! vars (car A) (list (cons val type)))
                  #t)))))

  (: assoc-check? (edge edge -> (Option edge)))
  (define (assoc-check? a b)
    ;; if a and b are ambiguous based on associativity, then return the
    ;; one that can be removed; otherwise, return #f.
    (define (td-check? A B)
      (let ([ac (edge-leaf-count A)]
            [bc (edge-leaf-count B)])
        (cond [(< ac bc) a]
              [(< bc ac) b]
              [else #f])))
    (match (cons a b)
      [(cons (edge i k A '() gamma '() assoc prec vars code src)
             (edge j l A* '() gamma* '() assoc* prec* vars* code* src*))
       (and (not (eq? a b))
            (equal? A A*)
            (not (eq? '⊥ assoc))
            (eq? assoc assoc*)
            (pair? src) (pair? src*)
            (eq? (cdr src) (cdr src*))
            (or (and (= 3 (length gamma))
                     (= 3 (length gamma*)))
                (and (= 2 (length gamma))
                     (= 2 (length gamma*))))
            (let ([B (first gamma)] [D (last gamma)]
                  [B* (first gamma*)] [D* (last gamma*)])
              (or (and (> k j) (> l k)
                       (equal? D B*)
                       (if (eq? assoc 'right) a b))
                  (and (> i j) (> l i)
                       (equal? B D*)
                       (if (eq? assoc 'right) b a))
                  (and (= i j) (= k l)
                       (if (eq? assoc 'right)
                           (td-check? D D*)
                           (td-check? B B*))))))]
      [else #f]))

  (: prec-check? (edge edge -> (Option edge)))
  (define (prec-check? a b)
    ;; if a and b are ambiguous based on precedence, then return the
    ;; one that can be removed; otherwise, return #f.
    (match (cons a b)
      [(cons (edge i k A '() gamma '() assoc prec vars code src)
             (edge j l A* '() gamma* '() assoc* prec* vars* code* src*))
       (and (not (eq? a b))
            (pair? src) (pair? src*)
            (eq? (car src) (car src*))
            (integer? prec) (integer? prec*)
            (or (and (= i j) (= k l)
                     (or (and (> prec prec*) a)
                         (and (> prec* prec) b)))
                (and (or (and (> k j) (> l k)
                              (equal? (last gamma) (first gamma*)))
                         (and (> i j) (> l i)
                              (equal? (first gamma) (last gamma*))))
                     (or (and (> prec prec*) b)
                         (and (> prec* prec) a)))))]
      [else #f]))

  (: spec-check? (edge edge -> (Option edge)))
  (define (spec-check? a b)
    (match (cons a b)
      [(cons (edge i k A '() gamma '() assoc prec vars code src)
             (edge j l A* '() gamma* '() assoc* prec* vars* code* src*))
       (and (not (eq? a b))
            (= i j) (= k l)
            (not (equal? a b))
            (or (and (more-specific? A A* grammar) b)
                (and (more-specific? A* A grammar) a)))]
      [else #f]))

  (: all-depends (edge -> (Listof edge)))
  (define (all-depends E)
    (let ([es (hash-ref chart-depend E (λ () '()))])
      (append es (apply append (map all-depends es)))))
  
  (: assoc/prec-chart-check!
     (edge (Listof edge) -> (Pairof (Setof edge) (Listof edge))))
  (define (assoc/prec-chart-check! e c)
    (let chart-check ([ret #f]
                      [cres (set)]
                      [c c])
      (if (null? c)
          (cons (if ret (set-add cres e) cres) (chart->list chart))
          (let ([f (car c)])
            (let ([a1 (assoc-check? e f)]
                  [p1 (prec-check? e f)])
              (cond [(or (eq? f a1) (eq? f p1))
                     (let ([deps (set-add (list->set (all-depends f)) f)])
                       (remove-from-chart! f 'cc)
                       (chart-check ret (set-union cres deps) (cdr c)))]
                    [(or (eq? e a1) (eq? e p1))
                     (chart-check #t cres (cdr c))]
                    [else (chart-check ret cres (cdr c))]))))))

  (: spec-chart-check!
     (edge (Listof edge) -> (Pairof (Setof edge) (Listof edge))))
  (define (spec-chart-check! e c)
    (: compl? (edge -> Boolean))
    (define (compl? f)
      (and (= (edge-start e) (edge-start f))
           (= (edge-end e) (edge-end f))
           (edge-complete? f)))
    (let ([c* (chart-filter compl? chart (edge-end e) 'end)])
      (let chart-check ([ret #f]
                        [cres (set)]
                        [c* c*])
        (if (null? c*)
            (cons (if ret (set-add cres e) cres) (chart->list chart))
            (let* ([f (car c*)]
                   [s1 (spec-check? e f)])
              (cond [(eq? f s1)
                     (let ([deps (set-add (list->set (all-depends f)) f)])
                       (remove-from-chart! f 'cc*)
                       (chart-check ret (set-union cres deps) (cdr c*)))]
                    [(eq? e s1)
                     (chart-check #t cres (cdr c*))]
                    [else (chart-check ret cres (cdr c*))]))))))
  
  (: amb-filter! ((Listof edge) -> (Listof edge)))
  (define (amb-filter! edges)
    ;; return the list of edges with ambiguous edges removed.
    ;; side effect: the aux agenda may be updated when spec? is #t.
    (: check-rec
       ((edge edge -> (Option edge)) edge (Setof edge) -> (Setof edge)))
    (define (check-rec check? e res)
      (let check ([ds edges])
        (if (null? ds)
            res
            (let ([r (check? e (car ds))])
              (if r (set-add res r) (check (cdr ds)))))))

    (: amb-edges
       ((edge edge -> (Option edge)) edge -> (Setof edge)))
    (define (amb-edges check? e)
      (let check ([ds edges]
                  [as '()]
                  [e-added? #f])
        (if (null? ds)
            (list->set as)
            (let ([r (check? e (car ds))])
              (if (and (edge? r) (eq? r e))
                  (if e-added?
                      (check (cdr ds) as e-added?)
                      (check (cdr ds) (cons r as) #t))
                  (if (edge? r)
                      (check (cdr ds) (cons r as) e-added?)
                      (check (cdr ds) as e-added?)))))))

    (let ([c (chart->list chart)])
      (let loop ([es edges]
                 [res (set)]
                 [ares (set)])
        (if (null? es)
            (begin (when trace?
                     (printf "ADD=~n")
                     (for ([r edges])
                       (unless (set-member? res r)
                         (printf "  ~a~n" (edge->string r))))
                     (printf "DEL=~n")
                     (for-each
                      (λ (r)
                        (printf "  ~a~n" (edge->string r)))
                      (set->list res)))
                   (add-to-aux! (set->list ares) 'amb)
                   (remove* (set->list res) edges))
            (let ([e (car es)])
              (unless (eq? '⊥ (edge-assoc e))
                (set! res (set-union res (amb-edges assoc-check? e))))
              (unless (eq? '⊥ (edge-prec e))
                (set! res (set-union res (amb-edges prec-check? e))))
              (when (or (not (eq? '⊥ (edge-assoc e)))
                        (integer? (edge-prec e)))
                (let ([cc (assoc/prec-chart-check! e c)])
                  (set! res (set-union res (car cc)))
                  (set! c (cdr cc))))
              (when (parser-ops-spec? ops)
                (let ([as (amb-edges spec-check? e)])
                  (set! res (set-union res as))
                  (add-to-aux! (set->list as) 'amb-amb))
                (let ([cc (spec-chart-check! e c)])
                  (set! res (set-union res (car cc)))
                  (set! ares (set-union ares (car cc)))
                  (set! c (cdr cc))))
              (loop (cdr es) res ares))))))

  (when trace?
    (printf "agenda=~n")
    (for ([a agenda])
      (pretty-print (agenda-state-edges a))))
  (let loop ()
    (let ([S (grammar-start grammar)])
      (match-let ([(agenda-state edges aux hook) (car agenda)])
        (if (null? edges)
            (if (null? (cdr agenda))
                (if (or (chart-has-parse? chart S) (null? aux))
                    (hook)
                    (let ([next (agenda-state edges (cdr aux) hook)]
                          [es (car aux)] [c (chart->list chart)])
                      (set! agenda (cons next (cdr agenda)))
                      (let check-aux ([es es] [c c])
                        (unless (null? es)
                          (let* ([e (car es)]
                                 [cc (assoc/prec-chart-check! e c)])
                            (unless (set-member? (car cc) e)
                              (when trace? (printf "AUX POP:~n  ")
                                (printf "(~a)~n  " (edge->string e)))
                              (process-edge! e))
                            (check-aux (cdr es) (cdr cc)))))
                      (loop)))
                (begin (set! agenda (cdr agenda))
                       (hook)
                       (loop)))
            ;; update aux variable because amb-filter! may change it
            (let* ([es (amb-filter! edges)]
                   [aux (agenda-state-aux (car agenda))]
                   [next (agenda-state '() aux hook)])
              (set! agenda (cons next (cdr agenda)))
              (for-each process-edge! es)
              (loop)))))))

(: initial-chart ((Vectorof token) Boolean grammar -> Chart))
(define (initial-chart tokens top-down? G)
  (if top-down?
      (td-initial-chart tokens G)
      (bu-initial-chart tokens G)))

(: initial-agenda ((Vectorof token) grammar LHS Boolean -> (Listof edge)))
(define (initial-agenda tokens grammar S top-down?)
  (if top-down?
      (td-initial-agenda grammar S)
      (bu-initial-agenda tokens grammar)))

(: td-initial-chart ((Vectorof token) grammar -> Chart))
(define (td-initial-chart tokens G)
  (let ([start (make-vector (+ 1 (vector-length tokens)) '())]
        [end (make-vector (+ 1 (vector-length tokens)) '())])
    (do ([i 0 (+ i 1)])
        ((= i (vector-length tokens)) (cons start end))
      (let* ([tok (vector-ref tokens i)]
             [val (token-value tok)]
             [j (+ i 1)]
             [src (cons (grammar-id G) (string->symbol val))]
             [e (edge i j val '() (list tok) '() '⊥ '⊥ (make-hash) #f src)])
        (vector-set! start (edge-start e) (list e))
        (vector-set! end (edge-end e) (list e))))))

(: td-initial-agenda (grammar LHS -> (Listof edge)))
(define (td-initial-agenda G S)
  (let ([src (cons (grammar-id G) 'start)])
    (list (edge 0 0 'S* '() '() `(,S) '⊥ '⊥ (make-hash) #f src))))

(: bu-initial-chart ((Vectorof token) grammar -> Chart))
(define (bu-initial-chart tokens G)
  (let ([start (make-vector (+ 1 (vector-length tokens)) '())]
        [end (make-vector (+ 1 (vector-length tokens)) '())])
    (do ([i 0 (+ i 1)])
        ((= i (vector-length tokens)) (cons start end))
      (vector-set! start i '())
      (vector-set! end (+ i 1) '()))))

(: bu-initial-agenda ((Vectorof token) grammar -> (Listof edge)))
(define (bu-initial-agenda tokens G)
  (let loop ([agenda '()] [i 0] [j 0])
    (if (= i (vector-length tokens))
        (reverse agenda)
        (let* ([tok (vector-ref tokens i)]
               [val (token-value tok)]
               [k (+ j 1)]
               [src (cons (grammar-id G) (string->symbol val))]
               [e (edge j k val '() (list tok) '() '⊥ '⊥ (make-hash) #f src)])
          (loop (cons e agenda) (+ i 1) (+ j 1))))))

(: param-subst
   (LHS (Listof Term) (Listof Edge-Term) (Listof Term)
        (HashTable LHS Any) -> LHS))
(define (param-subst A left found right vars)
  (or (and (null? left) (null? right)
           (let ([lhs (lookup vars A)])
             (and (rule-lhs? lhs) (not (string? lhs)) lhs)))
      A))

(: lexical? (edge -> Boolean))
(define (lexical? E)
  (and (edge-complete? E)
       (not (edge-code E))
       (list1? (edge-found E))
       (token? (car (edge-found E)))))

(: parsed-lexical? (edge -> Boolean))
(define (parsed-lexical? E)
  (and (edge-complete? E)
       (not (edge-code E))
       (list1? (edge-found E))
       (let ([p (car (edge-found E))])
         (and (pair? p)
              (string? (car p))
              (token? (cdr p))))))

(: compl-edge (edge -> (Listof Edge-Term)))
(define (compl-edge E)
  (or (and (lexical? E)
           (let ([t (car (edge-found E))])
             (and (token? t)
                  (list (cons (edge-lhs E) t)))))
      (or (and (parsed-lexical? E)
               (let ([p (car (edge-found E))])
                 (and (pair? p)
                      (token? (cdr p))
                      (list (cons (edge-lhs E) (cdr p))))))
          (list E))))

(: should-expand? (LHS grammar -> Boolean))
(define (should-expand? x grammar)
  (or (and (string? x)
           (is-derivation? x grammar))
      (and (rule-lhs? x)
           (not (string? x)))))

(: chart-filter
   ((edge -> Boolean) Chart Integer (U 'start 'end) -> (Listof edge)))
(define (chart-filter pred chart j pt)
  ;; pt is whether j is the start or end of an edge
  (if (eq? 'start pt)
      (filter pred (vector-ref (car chart) j))
      (filter pred (vector-ref (cdr chart) j))))

(: chart-text (Integer Chart -> String))
(define (chart-text i chart)
  (match (chart-filter lexical? chart i 'start)
    [(list (edge i j A '() (list (? token? t)) '() _ _ _ _ _))
     (token-value t)]
    ['() (error "no text in chart at" i)]
    [_ (error "ambiguous text in chart at" i)]))

(: chart->list (Chart -> (Listof edge)))
(define (chart->list chart)
  (apply append (vector->list (car chart))))

(: edge-is-parse? (edge Integer LHS -> Boolean))
(define (edge-is-parse? E n S)
  (and (edge-complete? E)
       (equal? S (edge-lhs E))
       (= n (edge-end E))
       (zero? (edge-start E))))

(: chart-has-parse? (Chart LHS -> Boolean))
(define (chart-has-parse? chart S)
  (let ([n (- (vector-length (cdr chart)) 1)])
    (not (null? (filter (λ (e)
                          (edge-is-parse? e n S))
                        (vector-ref (cdr chart) n))))))

(: parses
   ((Vectorof token) grammar (edge (Listof edge) -> (Listof edge)) LHS
    parser-ops -> (Listof edge)))
(define (parses tokens grammar enqueue S ops)
  (let* ([ops2 (parser-ops (parser-ops-init-state ops) #t
                           (parser-ops-left? ops)
                           (parser-ops-top-down? ops)
                           (parser-ops-spec? ops)
                           (parser-ops-trace? ops)
                           (parser-ops-report? ops))]
         [s (chart-parse tokens grammar enqueue S ops2)])
    (if (parser-state? s)
        (chart->parses (parser-state-chart s) S)
        (error "parses: expected state, got" s))))

(: chart->parses (Chart LHS -> (Listof edge)))
(define (chart->parses chart S)
  (define trees
    (filter (λ (e)
              (and (edge-complete? e)
                   (equal? S (edge-lhs e))
                   (zero? (edge-start e))))
            (vector-ref (cdr chart) (- (vector-length (cdr chart)) 1))))
  (when (null? trees)
    (let loop ([es (chart-filter
                    (λ (e)
                      (and (equal? S (edge-lhs e))
                           (edge-incomplete? e)
                           (not (null? (edge-right e)))))
                    chart 0 'start)]
               [errs '()])
      (if (null? es)
          (error 'syntax-error "~a error~a~a"
                 (length errs)
                 (if (list1? errs) "" "s")
                 (with-output-to-string
                   (λ ()
                     (for ([e errs])
                       (printf "\n~a" e)))))
          (let* ([e (car es)]
                 [t (get-last-token e)])
            (match (chart-filter lexical? chart (edge-end e) 'start)
              [(list (edge i j A '() (list (? token? u)) '() _ _ _ _ _))
               (if (token? t)
                   (loop
                    (cdr es)
                    (cons
                     (format
                      "~a:~a:~a: Expected ~a, but got \"~a\""
                      (token-source u)
                      (token-line u)
                      (token-column u)
                      (car (edge-right e))
                      (token-value u))
                     errs))
                   (loop (cdr es) errs))]
              [_
               (if (token? t)
                   (loop
                    (cdr es)
                    (cons (format "~a:~a:~a: Expected ~a after \"~a\""
                                  (token-source t)
                                  (token-line t)
                                  (token-column t)
                                  (car (edge-right e))
                                  (token-value t))
                          errs))
                   (loop (cdr es) errs))])))))
  trees)

(: report ((HashTable LHS Integer) -> Void))
(define (report stats)
  (let ([t 0]
        [r '(process-edge! ffr pred-td close-rec bfr pred-bu scan-bu)])
    (hash-for-each stats
      (λ (k v)
        (when (member k r)
          (set! t (+ t v)))
        #;(printf "~a => ~a~n" k v)))
    (printf " edges=~a" t)))

(: log-trace (Any Any -> Void))
(define (log-trace msg E)
  (printf "~a: ~a~n" msg (edge->string E)))
