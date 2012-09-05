;;;;;; es.rkt - Extensible Syntax.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 3 Sep 2012

#lang typed/racket/no-check

(require
 "bidi-edge.rkt"
 "grammar.rkt"
 "chart-parser.rkt"
 "lexer.rkt"
 "utils.rkt")

(provide parse-file/island parse-file/td-earley parse-file/bu-earley
         parse-file compile-file test *program*)

(define *def-reserved* '("forall" "module" "types"))
(define *use-reserved* '("declare" "import"))

(define-grammar *program*
  [Program ::= (Import Body)]
  [Import ::= ("import" NameList ";")]
  [Body ::= (Text+) (Declare) (Scope)]
  [Declare ::= (IDeclare (? Body))]
  [IDeclare ::= ((? Text+) "declare" Text+ "{" Body "}")]
  [Scope ::= (IScope (? Body))]
  [IScope ::= ((? Text+) "{" Body "}")]
  [NameList ::= (*Name) (*Name "," NameList)]
  [*Name ::= (,(match-token #rx"^[a-zA-Z][a-zA-Z0-9_>-]*$" *use-reserved*))]
  [Text ::= (,(match-token #rx"^[^{}]+$" *use-reserved*))])

(define-grammar *module-0*
  [Module ::= ("module" *Name "{" (? Text+) Types Text+)
          ("module" *Name "{" Text+)]
  [Types ::= ("types" "{" Rule+ "}")]
  [Rule ::= (ParamRule) (BasicRule) (TypeRule)]
  [ParamRule ::= ("forall" *Name+ "." NonParamRule)]
  [NonParamRule ::= (BasicRule) (TypeRule)]
  [BasicRule ::= (Type "::=" Expression ";")]
  [Expression ::= (Term+ (? Attr)) (Term+ (? Attr) "|" Expression)]
  [Term ::= (Type) (String) (RegExp)]
  [Attr ::= ("[" Attrs "]")]
  [Attrs ::= (Assoc) (Prec) (Assoc Prec)]
  [TypeRule ::= (Signature "==" SExp+ ";")]
  [Signature ::= (BasicSignature)]
  [BasicSignature ::= (Type "::=" Expression/Binding)]
  [Expression/Binding ::= (Term/Binding+ (? Attr))]
  [Term/Binding ::= (Term) (Binding)]
  [Binding ::= (*Name ":" Type)]
  [Type ::= (*Name)]
  [*Name ::= (,(match-token #rx"^[a-zA-Z][a-zA-Z0-9_>-]*$" *def-reserved*))]
  [SExp ::= (,(match-token #rx"^[^;]+$" *def-reserved*))]
  [Assoc ::= (,(match-token #rx"(left)|(right)"))]
  [Prec ::= (#rx"^[0-9]+$")]
  [String ::= (#rx"^[\"][^\"]+[\"]$")]
  [RegExp ::= (#rx"^#[rp]x\"(.*)\"$")]
  [Text ::= (,(match-token #rx"^.*$" '("types")))])

(: module-grammar ((Listof String) -> grammar))
(define (module-grammar *types*)
  (define match-name (mk-match-name *types*))
  (define-grammar *module*
    [Module ::= ("module" *Name "{" Rule+ "}")]
    [Rule ::= (ParamRule) (BasicRule) (ScopeRule) (FunRule) (MacRule)]
    [ParamRule ::= ("forall" *Name+ "." NonParamRule)]
    [NonParamRule ::= (BasicRule) (ScopeRule) (FunRule) (MacRule)]
    [BasicRule ::= (Type "::=" Expression ";")]
    [Expression ::= (Term+ (? Attr)) (Term+ (? Attr) "|" Expression)]
    [Term ::= (Type) (String) (RegExp)]
    [Attr ::= ("[" Attrs "]")]
    [Attrs ::= (Assoc) (Prec) (Assoc Prec)]
    [FunRule ::= (Signature "=" SExp+ ";")]
    [MacRule ::= (Signature "=>" SExp+ ";")]
    [TypeRule ::= (Signature "==" SExp+ ";")]
    [Signature ::= (BasicSignature) (ScopeSignature)]
    [BasicSignature ::= (Type "::=" Expression/Binding)]
    [ScopeSignature ::= (Type "::=" Term/Binding+ ScopeDef)]
    [ScopeDef ::= ("{" Binding+ ";" Term/Binding+ "}" (? String))]
    [ScopeRule ::= (ScopeSignature ";")]
    [Expression/Binding ::= (Term/Binding+ (? Attr))]
    [Term/Binding ::= (Term) (Binding)]
    [Binding ::= (*Name ":" Type)]
    [Type ::= (*Name) ,TypeParen]
    [*Name ::= (,(match-name #rx"^[a-zA-Z][a-zA-Z0-9_>-]*$" *def-reserved*))]
    [SExp ::= (,(match-token #rx"^[^;]+$" *def-reserved*))]
    [Assoc ::= (,(match-token #rx"(left)|(right)"))]
    [Prec ::= (#rx"^[0-9]+$")]
    [String ::= (#rx"^[\"][^\"]+[\"]$")]
    [RegExp ::= (#rx"^#[rp]x\"(.*)\"$")])
  *module*)

(: declare-grammar ((Option grammar) (Listof String) -> grammar))
(define (declare-grammar G *types*)
  (define match-name (mk-match-name *types*))
  (define-grammar *declare*
    [BindingList ::= (Binding) (Binding "," BindingList)]
    [Binding ::= (*Name ":" Type)]
    [Type ::= (*Name) ,TypeParen]
    [*Name ::= (,(match-name #rx"^[a-zA-Z][a-zA-Z0-9_>-]*$" *use-reserved*))])
  (if (grammar? G)
      (grammar-union *declare* G)
      *declare*))

(: body-grammar
   (grammar (Option grammar) (Option (Listof String)) -> grammar))
(define (body-grammar G T *types*)
  (define match-name (mk-match-name (or *types* '())))
  (define-grammar *base-type*
    [Type ::= (*Name) ,TypeParen]
    [*Name ::= (,(match-name #rx"^[a-zA-Z][a-zA-Z0-9_>-]*$" *use-reserved*))])
  (grammar-union G (if (grammar? T)
                       (grammar-union *base-type* T)
                       *base-type*)))

(define TypeParen
  (let ([rhs '("(" ("T" . Type) ")")]
        [vars (make-hash)]
        [type '(T)]
        [proc (λ: ([args : (Listof Any)])
                (eval `(let-syntax ([eval (syntax-rules ()
                                            [(_ T) 'T])])
                         (eval ,@args))
                      (make-base-namespace)))])
    (hash-set! vars "T" '(#f . Type))
    (half-rule rhs '⊥ '⊥ vars (cons type proc))))

(: mk-match-name
   ((Listof String) ->
    (case-> [Regexp -> (String -> (Option String))]
            [Regexp (Listof String) -> (String -> (Option String))])))
(define (mk-match-name *types*)
  (let ([mn (λ: ([regexp : Regexp] [except : (Listof String)])
              (λ: ([str : String])
                (and (regexp-match regexp str)
                     (not (member str except))
                     (let: loop : (Option String)
                           ([types : (Listof String) *types*])
                       (or (and (null? types) str)
                           (let ([t (car types)])
                             (and (if (regexp? t)
                                      (not (regexp-match t str))
                                      (not (equal? t str)))
                                  (loop (cdr types)))))))))])
    (case-lambda:
     [([regexp : Regexp])
      (mn regexp '())]
     [([regexp : Regexp] [except : (Listof String)])
      (mn regexp except)])))

;; set at runtime: whether to compile or parse, i.e.,
;;   if a grammar rule has an implementation, compile
(define: *compile-mode?* : Boolean #f)

;; whether to parse bidirectionally, i.e.,
;;   allow symbols to the left of an edge's first dot
(define: *bidi-mode?* : Boolean #t)

;; whether to parse bottom-up, i.e.,
;;   #t => agenda_0 = tokens; #f => chart_0 = tokens
(define: *bottom-up-mode?* : Boolean #t)

;; whether to print a trace when parsing the program
(define: *inner-trace?* : Boolean #f)

;; whether to report the parse time and edge count
(define: *inner-report?* : Boolean #f)

;; whether or not to include terminals in AST, e.g.,
;;   A ::= B "+" C; => (A (B "+" C)) or (A (B C))
(define: *edge->ast-terminals?* : Boolean #f)

(define-type Env (Rec E (U #f (Pairof (HashTable String Any) E))))
(define-predicate env? Env)

(struct: es-ops
  ([state : (Option parser-state)]
   [ret-state? : Boolean]
   [env : Env]
   [code? : Boolean]
   [types : (Option (Pairof grammar (Listof String)))]))

(: compile-file
   (case-> [String -> Void]
           [String Boolean -> Void]
           [String Boolean Boolean -> Void]))
(define compile-file
  (let ([cf (λ: ([filename : String] [trace? : Boolean] [time? : Boolean])
              (let ([out (format "~a.rkt" (filename-base filename))]
                    [ps (parse-file/bu-earley filename trace? time? #t)])
                (cond [(null? ps) (error "parse error")]
                      [(list1? ps)
                       (with-output-to-file out
                         (λ ()
                           (printf "#lang typed/racket/no-check~n")
                           (let ([p (car ps)])
                             (when (list? p)
                               (for-each (λ (e) (printf "~s~n" e)) p))))
                         #:exists 'replace)]
                      [else (error "parse ambiguous")])))])
    (case-lambda:
     [([filename : String])
      (cf filename #f #f)]
     [([filename : String] [trace? : Boolean])
      (cf filename trace? #f)]
     [([filename : String] [trace? : Boolean] [time? : Boolean])
      (cf filename trace? time?)])))

(define-syntax-rule (mk-parse-file body ...)
  (let ([pf (λ: ([filename : String] [trace? : Boolean] [time? : Boolean]
                 [compile? : Boolean])
              body ...
              (parse-file filename trace? time? compile?))])
    (case-lambda:
     [([filename : String])
      (pf filename #f #f #f)]
     [([filename : String] [trace? : Boolean])
      (pf filename trace? #f #f)]
     [([filename : String] [trace? : Boolean] [time? : Boolean])
      (pf filename trace? time? #f)]
     [([filename : String] [trace? : Boolean] [time? : Boolean]
       [compile? : Boolean])
      (pf filename trace? time? compile?)])))

(: parse-file/island
   (case-> [String -> (U (Listof Sexp) (Listof edge))]
           [String Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean Boolean -> (U (Listof Sexp) (Listof edge))]))
(define parse-file/island
  (mk-parse-file
   (set! *bidi-mode?* #t)
   (set! *bottom-up-mode?* #t)))

(: parse-file/bu-earley
   (case-> [String -> (U (Listof Sexp) (Listof edge))]
           [String Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean Boolean -> (U (Listof Sexp) (Listof edge))]))
(define parse-file/bu-earley
  (mk-parse-file
   (set! *bidi-mode?* #f)
   (set! *bottom-up-mode?* #t)))

(: parse-file/td-earley
   (case-> [String -> (U (Listof Sexp) (Listof edge))]
           [String Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean Boolean -> (U (Listof Sexp) (Listof edge))]))
(define parse-file/td-earley
  (mk-parse-file
   (set! *bidi-mode?* #f)
   (set! *bottom-up-mode?* #f)))

(: parse-file
   (case-> [String -> (U (Listof Sexp) (Listof edge))]
           [String Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean -> (U (Listof Sexp) (Listof edge))]
           [String Boolean Boolean Boolean -> (U (Listof Sexp) (Listof edge))]))
(define parse-file
  (let ([pf (λ: ([filename : String] [trace? : Boolean] [time? : Boolean]
                 [compile? : Boolean])
              (set! *compile-mode?* compile?)
              (set! *inner-trace?* trace?)
              (set! *inner-report?* time?)
              (let* ([tokens (tokenize-file filename)]
                     [ops (parser-ops #f #f #f #t #f #f #f)]
                     [ps (parses tokens *program* cons 'Program ops)])
                (cond [(null? ps) (error "surface syntax error!")]
                      [(list1? ps)
                       (parse-program (car ps) filename)]
                      [else
                       (error (format "surface syntax ambiguous! (~a)"
                                      (length ps)))])))])
    (case-lambda:
     [([filename : String])
      (pf filename #f #f #f)]
     [([filename : String] [trace? : Boolean])
      (pf filename trace? #f #f)]
     [([filename : String] [trace? : Boolean] [time? : Boolean])
      (pf filename trace? time? #f)]
     [([filename : String] [trace? : Boolean] [time? : Boolean]
       [compile? : Boolean])
      (pf filename trace? time? compile?)])))

(: parse-file/module
   (String -> (List grammar (Option (Pairof grammar (Listof String))))))
(define (parse-file/module filename)
  (let* ([tokens (tokenize-file filename)]
         [ops (parser-ops #f #f #f #t #f #f #f)]
         [ps (parses tokens *module-0* cons (grammar-start *module-0*) ops)])
    (cond
     [(null? ps)
      (error (format "module ~a syntax error!" filename))]
     [(list1? ps)
      (let-values ([(G H) (parse-module-0 (car ps) filename)])
        (list G H))]
     [else
      (error (format "module ~a syntax ambiguous (~a)!"
                     filename (length ps)))])))

(: parse-program (edge String -> (U (Listof Sexp) (Listof edge))))
(define (parse-program E filename)
  ;; E : [Program ::= (Import Body)]
  (match E
    [(edge-complete 'Program (list (? edge? Import) (? edge? Body)))
     (let* ([GT (parse-import Import filename)]
            [T (and (second GT) (car (second GT)))]
            [*types* (and (second GT) (or (cdr (second GT)) '()))]
            [G (body-grammar (first GT) T *types*)]
            [ops (es-ops #f #f #f #t (second GT))]
            [ps (parse-body Body G ops)])
       (if (parser-state? ps)
           (error "expected Sexp:" ps)
           ps))]))

(: parse-import
   (edge String -> (List grammar (Option (Pairof grammar (Listof String))))))
(define (parse-import E filename)
  ;; E : [Import ::= ("import" NameList ";")]
  (match E
    [(edge-complete 'Import (list _ (? edge? NameList) _))
     (let* ([dir (filename-dir filename)]
            [parse-name
             (λ: ([p : Edge-Term])
               (match p
                 [(cons '*Name (? token? t)) (token-value t)]))]
            [names (parse-rec NameList parse-name first (mk-rec third))]
            [mods (map (λ (n) (format "~a~a.es" dir n)) names)])
       (foldl
        (λ: ([x : (List grammar (Option (Pairof grammar (Listof String))))]
             [y : (List grammar (Option (Pairof grammar (Listof String))))])
          (let ([G (grammar-union (first y) (first x))])
            (if (second x)
                (let ([H (car (second x))] [T (cdr (second x))])
                  (if (second y)
                      (list G (cons (grammar-union (car (second y)) H)
                                    (append (cdr (second y)) T)))
                      (list G (second x))))
                (list G (second y)))))
        (list (mk-grammar '() 'G0) #f)
        (map parse-file/module mods)))]))

(: parse-body
   (edge grammar es-ops -> (U parser-state (Listof Sexp) (Listof edge))))
(define (parse-body E G ops)
  ;; E : [Body ::= Text+ Declare Scope]
  (match E
    [(edge-complete 'Body (list (? edge? e)))
     (let ([state (es-ops-state ops)]
           [ret-state? (es-ops-ret-state? ops)]
           [env (es-ops-env ops)]
           [code? (es-ops-code? ops)]
           [T (es-ops-types ops)])
       (case (edge-lhs e)
         [(Text+)
          (let ([s1 (next-state state E G env #f)])
            (if *compile-mode?*
                (compile-text e G s1 ret-state? code?)
                (parse-text e G s1 ret-state? #t)))]
         [(Declare) (parse-declare e G ops)]
         [(Scope) (parse-scope e G ops)]
         [else (error "parse-body: unexpected:" (edge-lhs e))]))]))

(: parse-declare
   (edge grammar es-ops -> (U parser-state (Listof Sexp) (Listof edge))))
(define (parse-declare E G ops)
  ;; E : [Declare ::= (IDeclare (? Body))]
  (let ([decl (first (edge-found E))]
        [state (es-ops-state ops)]
        [ret-state? (es-ops-ret-state? ops)]
        [env (es-ops-env ops)]
        [code? (es-ops-code? ops)]
        [T (es-ops-types ops)])
    (let-values ([(id H) (parse-ideclare decl G (es-ops state #t env #f T))])
      (cond
       [(parser-state? id)
        (state/undeclare! id H)
        (if (list1? (edge-found E))
            (cond [(pair? env) id]
                  [*compile-mode?* (compile-state id G)]
                  [ret-state? id]
                  [else (close-state id G)])
            (let* ([bod (second (edge-found E))]
                   [s1 (next-state id bod G env #f)])
              (parse-body bod G (es-ops s1 ret-state? env code? T))))]
       [else (error "expected state:" id)]))))

(: parse-ideclare
   (edge grammar es-ops ->
         (Values (U parser-state (Listof Sexp) (Listof edge)) grammar)))
(define (parse-ideclare E G ops)
  ;; E : [IDeclare ::= ((? Text+) "declare" Text+ "{" Body "}")]
  (define (bindings)
    (let* ([words (extract-text
                   (if (listn? 6 (edge-found E))
                       (third (edge-found E))
                       (second (edge-found E))))]
           [T (es-ops-types ops)]
           [D (declare-grammar (if T (car T) #f) (if T (cdr T) '()))]
           [ps (parse-words words D #f #f #f)])
      (if (list? ps)
          (car ps)
          (error (format "syntax error: declare~a"
                         (if (null? ps) "" " ambiguous"))))))
  (define (body)
    (if (listn? 6 (edge-found E))
        (fifth (edge-found E))
        (fourth (edge-found E))))
  (define (first-state)
    (if (listn? 6 (edge-found E))
        (let* ([pre (first (edge-found E))]
               [s0 (next-state (es-ops-state ops) pre G (es-ops-env ops) #f)])
          (if *compile-mode?*
              (compile-text pre G s0 #t #f)
              (parse-text pre G s0 #t #f)))
        (es-ops-state ops)))
  (let loop ([bs (parse-binding-list (bindings))]
             [rules '()])
    (if (null? bs)
        (let* ([H (mk-grammar rules 'H)]
               [I (grammar-union G H)]
               [ret-state? (es-ops-ret-state? ops)]
               [env (es-ops-env ops)]
               [code? (es-ops-code? ops)]
               [s1 (next-state/declare (first-state) (body) I H env #f)]
               [ops-b (es-ops s1 ret-state? env code? #f)])
          (values (parse-body (body) I ops-b) H))
        (match (car bs)
          [(cons (? string? rhs) (? rule-lhs? lhs))
           (loop (cdr bs) (cons `(,lhs ::= (,rhs)) rules))]))))

(: parse-scope
   (edge grammar es-ops -> (U parser-state (Listof Sexp) (Listof edge))))
(define (parse-scope E G ops)
  ;; E : [Scope ::= (IScope (? Body))]
  (let* ([iscope (first (edge-found E))]
         [state (es-ops-state ops)]
         [ret-state? (es-ops-ret-state? ops)]
         [env (es-ops-env ops)]
         [code? (es-ops-code? ops)]
         [T (es-ops-types ops)]
         [ops-is (es-ops state #t env code? T)]
         [is (parse-iscope iscope G ops-is)])
    (if (parser-state? is)
        (if (list1? (edge-found E))
            (cond [(pair? env) is]
                  [*compile-mode?* (compile-state is G)]
                  [ret-state? is]
                  [else (close-state is G)])
            (let ([ops (es-ops is (es-ops-ret-state? ops) env code? T)])
              (parse-body (second (edge-found E)) G ops)))
        (error "expected parser-state:" is))))

(: parse-iscope
   (edge grammar es-ops -> (U parser-state (Listof Sexp) (Listof edge))))
(define (parse-iscope E G ops)
  ;; E : [IScope ::= ((? Text+) "{" Body "}")])
  (define open-scope
    (if (list4? (edge-found E))
        (match (second (edge-found E))
          [(cons _ (? token? t)) (vector t)])
        (match (first (edge-found E))
          [(cons _ (? token? t)) (vector t)])))
  (define close-scope
    (match (last (edge-found E))
      [(cons _ (? token? t)) (vector t)]))
  (define body
    (if (list4? (edge-found E))
        (third (edge-found E))
        (second (edge-found E))))
  (define (first-state)
    (let ([state (es-ops-state ops)])
      (if (list4? (edge-found E))
          (let* ([pre (extract-text (first (edge-found E)))]
                 [env (es-ops-env ops)]
                 [s0 (next-state-tokens state pre G env #t)])
            (parse-words pre G s0 #t #t))
          state)))
  (: first-edges ((Option parser-state) -> (Listof edge)))
  (define (first-edges fs)
    (: pre-scope? (edge -> Boolean))
    (define (pre-scope? e)
      (and (edge-incomplete? e)
           (null? (edge-left e))
           (equal? "{" (first (edge-right e)))))
    (if (parser-state? fs)
        (let ([end (- (vector-length (cdr (parser-state-chart fs))) 1)])
          (chart-filter pre-scope? (parser-state-chart fs) end 'end))
        '()))
  (let* ([s1 (first-state)]
         [s2 (next-state-tokens s1 open-scope G #f #f)]
         [s3 (parse-words open-scope G s2 #t #t)]
         [env (make-env (es-ops-env ops) (map edge-vars (first-edges s1)))]
         [ops-b (es-ops s3 #t env #f #f)]
         [s4 (parse-body body G ops-b)]
         [s5 (next-state-tokens s4 close-scope G #f #f)])
    (parse-words close-scope G s5 (es-ops-ret-state? ops) #t)))

(: parse-text
   (edge grammar (Option parser-state) Boolean Boolean ->
         (U parser-state (Listof Sexp) (Listof edge))))
(define (parse-text E G state ret-state? ast?)
  ;; E : [Text+ ::= (Text (? Text+))]
  (parse-words (extract-text E) G state ret-state? ast?))

(: parse-words
   ((Vectorof token) grammar (Option parser-state) Boolean Boolean ->
    (U parser-state (Listof Sexp) (Listof edge))))
(define (parse-words tokens G state ret-state? ast?)
  (let ([S (grammar-start G)]
        [enqueue (λ: ([e : edge] [a : (Listof edge)]) (append a (list e)))]
        [left? *bidi-mode?*] [top-down? (not *bottom-up-mode?*)]
        [spec? #t] [trace? *inner-trace?*] [report? *inner-report?*])
    (when trace? (pretty-print G))
    (if ret-state?
        (let ([ops (parser-ops state #t left? top-down? spec? trace? report?)])
          (chart-parse tokens G enqueue S ops))
        (let* ([ops (parser-ops state #f left? top-down? spec? trace? report?)]
               [edges (parses tokens G enqueue S ops)])
          (if ast?
              (map (λ: ([e : edge]) (edge->ast e *edge->ast-terminals?*))
                   edges)
              edges)))))

(: compile-text
   (edge grammar (Option parser-state) Boolean Boolean ->
         (U parser-state (Listof Sexp) (Listof edge))))
(define (compile-text E G state ret-state? code?)
  ;; E : [Text+ ::= (Text (? Text+))]
  (let ([code-state (parse-words (extract-text E) G state #t #t)])
    (if code?
        (compile-state code-state G)
        code-state)))

(: compile-state (parser-state grammar -> (U (Listof Sexp) (Listof edge))))
(define (compile-state state G)
  (match-let ([(cons args fdefs) (gen-fun-defs G)])
    (let loop ([i 0] [ws '()] [fail? #t])
      (if (= i (- (vector-length (car (parser-state-chart state))) 1))
          (if fail?
              (error "compile error! no code")
              (let ([body (with-input-from-string
                              (format "(begin ~a)"
                                      (string-join (reverse ws) " "))
                            read)])
                (list (append (apply append fdefs)
                              (list body)))))
          (let* ([chart (parser-state-chart state)]
                 [es (chart-filter (fun-parse? args) chart i 'start)])
            (if (null? es)
                (let ([w (chart-text i chart)])
                  (loop (+ i 1) (cons w ws) fail?))
                (loop (edge-end (car es))
                      (let ([code (compile-code (car es) args)])
                        (append (reverse code) ws))
                      #f)))))))

(: compile-code
   ((U Edge-Term Term) (HashTable Symbol (Option (Listof Symbol))) ->
    (Listof String)))
(define (compile-code c args)
  (: mk-args (Symbol -> (Listof String)))
  (define (mk-args id)
    (let ([lst (hash-ref args id)])
      (if lst
          (map symbol->string lst)
          (error "expected (Listof Symbol):" lst))))
  (cond [(edge? c)
         (let ([vars (edge-vars c)]
               [id (cdr (edge-src c))])
           (match (hash-ref args id (λ () #f))
             [(? list? formals)
              (let loop ([fs (map symbol->string formals)]
                         [ss '()])
                (if (null? fs)
                    `("(" ,(symbol->string id) ,@ss ")")
                    (let* ([f (car fs)]
                           [k (if (hash-has-key? vars f) f
                                  (string->symbol f))])
                      (match (hash-ref vars k)
                        [(or (? rule-lhs? sub-code)
                             (list (cons sub-code _)))
                         (if (or (edge? sub-code)
                                 (sexpr? sub-code)
                                 (string? sub-code))
                             (loop (cdr fs)
                                   (append ss (compile-code sub-code args)))
                             (error "compile-code unexpected:" sub-code))]))))]
             [else (list (unparse (if (eq? 'Type (edge-lhs c))
                                      (parse-type c) c) " "))]))]
        [else (list (unparse c " "))]))

(: gen-fun-defs
   (grammar -> (Pairof (HashTable Symbol (Option (Listof Symbol)))
                       (Listof (Listof Sexp)))))
(define (gen-fun-defs G)
  ;; returns (cons args defs) where args maps rule ids to argument lists
  ;; and defs is a list of function and macro defns
  (let* ([args (make-hash)]
         [all-rules (expand-rules (grammar-rules G))]
         [defs (map (λ: ([r : rule])
                      (let ([id (cdr (rule-src r))]
                            [code (rule-code r)])
                        ;; code = (type . defn), type = '() for macros
                        ;; defn = #<procedure>
                        ;;      | (lambda formals body)
                        ;;      | (syntax-rules () [(_ pat) exp])
                        (cond [(procedure? (cdr code)) ;; type-rule
                               (hash-set! args id #f) #f]
                              [(car code)              ;; function/macro
                               (match (cdr code)
                                 [(rule-function formals body)
                                  (hash-set! args id formals)
                                  `((: ,id ,(car code))
                                    (define ,id ,(cdr code)))]
                                 [(rule-macro pat exp)
                                  (hash-set! args id pat)
                                  `((define-syntax ,id ,(cdr code)))])]
                              [else (error "gen-fun-defs unexpected:" code)])))
                    (filter fun-def? all-rules))])
    (cons args (filter sexp-list? defs))))

(define-match-expander rule-function
  (syntax-rules ()
    [(rule-function formals body)
     `(λ ,(? symbol-list? formals) ,body)]))

(define-match-expander rule-macro
  (syntax-rules ()
    [(rule-macro pat exp)
     `(syntax-rules ()
        [,(cons _ (? symbol-list? pat)) ,exp])]))

(: parse-module-0
   (edge String -> (Values grammar (Option (Pairof grammar (Listof String))))))
(define (parse-module-0 E filename)
  ;; E : [Module ::= ("module" *Name "{" (? Text+) (? Types) Text+)]
  (: grammar-words
     (-> (Values grammar (Option (Pairof grammar (Listof String)))
                 (Vectorof token))))
  (define (grammar-words)
    (define: start-words : (Vectorof token)
      (match (edge-found E)
        [(list (cons _ (? token? module)) (cons '*Name (? token? name))
               (cons _ (? token? open-module)) _ ...)
         (vector module name open-module)]))
    (match (length (edge-found E))
      [6
       (let ([t1 (extract-text (fourth (edge-found E)))]
             [T (parse-types (fifth (edge-found E)))]
             [t2 (extract-text (sixth (edge-found E)))])
         (values (grammar-union (module-grammar (cdr T)) (car T)) T
                 (vector-append start-words t1 t2)))]
      [5
       (let ([T (parse-types (fourth (edge-found E)))]
             [t1 (extract-text (fifth (edge-found E)))])
         (values (grammar-union (module-grammar (cdr T)) (car T)) T
                 (vector-append start-words t1)))]
      [4
       (let ([t1 (extract-text (fourth (edge-found E)))])
         (values (module-grammar '()) #f
                 (vector-append start-words t1)))]))
  (let-values ([(G H words) (grammar-words)])
    (let* ([trace? #f]
           [ops (parser-ops #f #f #f #t #f trace? #f)])
      (when trace? (pretty-print G))
      (let ([ps (parses words G cons (grammar-start G) ops)])
        (cond
         [(null? ps) (error (format "module-0 ~a syntax error!" filename))]
         [(list1? ps) (values (parse-module (car ps)) H)]
         [else (error (format "module-0 ~a syntax ambiguous (~a)!"
                              filename (length ps)))])))))

(: parse-types (edge -> (Pairof grammar (Listof String))))
(define (parse-types E)
  ;; E : [Types ::= ("types" "{" Rule+ "}")]
  (let ([rules (parse-rule+ (third (edge-found E)))])
    (let split ([R rules]
                [rules '()]
                [types '()])
      (cond [(null? R)
             (cons (mk-grammar (reverse rules) 'Types) types)]
            [(lexical-rule? (car R))
             (let ([ts (match (get-lexical-term (car R))
                         [(? string? t) (list t)]
                         [_ '()])])
               (split (cdr R) (cons (car R) rules) (append types ts)))]
            [else (split (cdr R) (cons (car R) rules) types)]))))

(: parse-module (edge -> grammar))
(define (parse-module E)
  ;; E : [Module ::= ("module" *Name "{" Rule+ "}")]
  (let ([name (string->symbol (unparse (second (edge-found E))))]
        [rules (parse-rule+ (fourth (edge-found E)))])
    (let split ([R rules] [rules '()])
      (if (null? R)
          (mk-grammar (reverse rules) name)
          (split (cdr R) (cons (car R) rules))))))

(: parse-rule (edge -> (Listof Rule-Spec)))
(define (parse-rule E)
  ;; E : [Rule ::= ParamRule BasicRule ScopeRule FunRule MacRule TypeRule]
  (let ([e (first (edge-found E))])
    (match (edge-lhs e)
      ['ParamRule (parse-param-rule e)]
      ['BasicRule (parse-basic-rule e '())]
      ['ScopeRule (parse-scope-rule e '())]
      ['FunRule (parse-fun-rule e '())]
      ['MacRule (parse-mac-rule e '())]
      ['TypeRule (parse-type-rule e '())])))

(: parse-param-rule (edge -> (Listof Rule-Spec)))
(define (parse-param-rule E)
  ;; E : [ParamRule ::= ("forall" *Name+ "." NonParamRule)]
  ;;   where [NonParamRule ::= BasicRule ScopeRule FunRule MacRule]
  (: adjust (String -> (Pairof Symbol Any)))
  (define (adjust var) (cons (string->symbol var) #f))
  (let* ([*Name+ (second (edge-found E))]
         [parse-name
          (λ: ([p : Edge-Term])
            (match p
              [(cons '*Name (? token? t)) (token-value t)]))]
         ;;[(edge-complete '*Name (list (cons _ (? string? n)))) n]))]
         [vars (parse-rec *Name+ parse-name first (mk-rec second))]
         [tbsrule (fourth (edge-found E))])
    ((match (edge-lhs (first (edge-found tbsrule)))
       ['BasicRule parse-basic-rule]
       ['ScopeRule parse-scope-rule]
       ['FunRule parse-fun-rule]
       ['MacRule parse-mac-rule]
       ['TypeRule parse-type-rule])
     (first (edge-found tbsrule))
     (map adjust vars))))

(: parse-basic-rule (edge (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-basic-rule E vars)
  ;; E : [BasicRule ::= (Type "::=" Expression ";")]
  (let ([type (parse-type (first (edge-found E)))])
    (map (λ: ([hr : half-rule])
           ;; TODO: make this cleaner somehow
           (let ([lhs type]
                 [rhs (list hr)])
             (cons lhs (cons '::= rhs))))
         (parse-expression (third (edge-found E)) vars))))

(: parse-expression (edge (Listof (Pairof LHS Any)) -> (Listof half-rule)))
(define (parse-expression E vars)
  ;; E : [Expression ::= (Term+ Attr?) (Term+ Attr? "|" Expression)]
  (let loop ([E E] [rs '()])
    (let ([e (parse-term+ (first (edge-found E)))]
          [vs (make-hash vars)])
      (cond [(list1? (edge-found E))
             (reverse (cons (half-rule e '⊥ '⊥ vs #f) rs))]
            [(list2? (edge-found E))
             (let ([attr (second (edge-found E))])
               (let-values ([(assoc prec) (parse-attr attr)])
                 (reverse (cons (half-rule e assoc prec vs #f) rs))))]
            [(list3? (edge-found E))
             (let ([F (third (edge-found E))])
               (loop F (cons (half-rule e '⊥ '⊥ vs #f) rs)))]
            [else
             (let ([attr (second (edge-found E))]
                   [F (fourth (edge-found E))])
               (let-values ([(assoc prec) (parse-attr attr)])
                 (loop F (cons (half-rule e assoc prec vs #f) rs))))]))))

(: parse-term (Edge-Term -> Term))
(define (parse-term E)
  ;; E : [Term ::= Type String RegExp] or [Type String RegExp]
  (let ([t (if (edge? E) (first (edge-found E)) E)])
    (match t
      [(? edge? term)
       (parse-type term)]
      [(cons 'String t)
       (let ([s (token-value t)])
         (substring s 1 (- (string-length s) 1)))]
      [(cons 'RegExp t)
       (let ([s (token-value t)])
         (regexp (substring s 4 (- (string-length s) 1))))]
      [_ (error (format "parse-term unexpected: ~a" E))])))

(: parse-attr (edge -> (Values Assoc Prec)))
(define (parse-attr E)
  ;; E : [Attr ::= "[" Attrs "]"] where
  ;;   [Attrs ::= Assoc Prec (Assoc Prec)]
  (let* ([e (second (edge-found E))]
         [fst (first (edge-found e))]
         [fst-str (unparse fst)])
    (cond [(list2? (edge-found e))
           (let ([snd-str (unparse (second (edge-found e)))])
             (values (string->symbol fst-str)
                     (string->number snd-str)))]
          [(eq? 'Assoc (car fst))
           (values (string->symbol fst-str) '⊥)]
          [else (values '⊥ (string->number fst-str))])))

(: parse-fun-rule (edge (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-fun-rule E vars)
  ;; E : [FunRule ::= (Signature "=" SExp+ ";")]
  (: mk-type (LHS (Listof (Pairof Symbol Any)) -> SExpr))
  (define (mk-type lhs formals)
    (let* ([kar (λ: ([p : (Pairof Symbol Any)]) (car p))]
           [kdr (λ: ([p : (Pairof Symbol Any)])
                  (cdr p))]
           [type `(,@(map kdr formals) -> ,lhs)]
           [types (list->set (flatten type))]
           [var-types (map kar vars)])
      (let close-type ([var-types var-types]
                       [fvs '()])
        (if (null? var-types)
            (or (and (null? fvs) type)
                `(All ,(reverse fvs) ,type))
            (let ([var (car var-types)])
              (if (set-member? types var)
                  (close-type (cdr var-types) (cons var fvs))
                  (close-type (cdr var-types) fvs)))))))
  (: mk-mk-code
     (Sexp -> (LHS (Listof (Pairof Symbol Any)) ->
                   (U Sexp ((Listof Any) -> Sexp)))))
  (define (mk-mk-code body)
    (let ([kar (λ: ([p : (Pairof Symbol Any)]) (car p))])
      (λ: ([lhs : LHS] [formals : (Listof (Pairof Symbol Any))])
        `(λ ,(map kar formals) ,body))))
  (parse-rule-action E mk-type mk-mk-code vars))

(: parse-mac-rule (edge (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-mac-rule E vars)
  ;; E : [MacRule ::= (Signature "=>" SExp+ ";")]
  (: mk-type (LHS (Listof (Pairof Symbol Any)) -> SExpr))
  (define (mk-type lhs formals) '())
  (: mk-mk-code
     (Sexp -> (LHS (Listof (Pairof Symbol Any)) ->
                   (U Sexp ((Listof Any) -> Sexp)))))
  (define (mk-mk-code body)
    (let ([kar (λ: ([p : (Pairof Symbol Any)]) (car p))])
      (λ: ([lhs : LHS] [formals : (Listof (Pairof Symbol Any))])
        (let ([types (map kar vars)])
          `(syntax-rules ()
             [(_ ,@(map kar formals) ,@types) ,body])))))
  (parse-rule-action E mk-type mk-mk-code vars))

;;; parse-type-rule : Edge -> Rule
;; Edge : [TypeRule ::= (Signature "==" SExp+ ";")]
(: parse-type-rule (edge (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-type-rule E vars)
  (: mk-type (LHS (Listof (Pairof Symbol Any)) -> SExpr))
  (define (mk-type lhs formals)
    (mapcar formals))
  (: mk-mk-code
     (Sexp -> (LHS (Listof (Pairof Symbol Any)) ->
                   (U Sexp ((Listof Any) -> Sexp)))))
  (define (mk-mk-code body)
    (λ: ([lhs : LHS] [formals : (Listof (Pairof Symbol Any))])
      (λ: ([args : (Listof Any)])
        (eval `(let-syntax ([eval (syntax-rules ()
                                    [(_ ,@(mapcar formals))
                                     ',body])])
                 (eval ,@args))
              (make-base-namespace)))))
  (parse-rule-action E mk-type mk-mk-code vars))

(: parse-rule-action
   (edge (LHS (Listof (Pairof Symbol Any)) -> SExpr)
         (Sexp -> (LHS (Listof (Pairof Symbol Any)) ->
                       (U Sexp ((Listof Any) -> Sexp))))
         (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-rule-action E mk-type mk-mk-code vars)
  (let ([sig (first (edge-found E))]
        [body (parse-sexp+ (third (edge-found E)))])
    (parse-signature sig mk-type (mk-mk-code body) vars)))

(: parse-signature
   (edge (LHS (Listof (Pairof Symbol Any)) -> SExpr)
         (LHS (Listof (Pairof Symbol Any)) -> (U Sexp ((Listof Any) -> Sexp)))
         (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-signature E mk-type mk-code vars)
  ;; E : [Signature ::= BasicSignature ScopeSignature]
  (let ([sig (first (edge-found E))])
    ((if (eq? 'BasicSignature (edge-lhs sig))
         parse-basic-signature parse-scope-signature)
     sig mk-type mk-code vars)))

(: parse-basic-signature
   (edge (LHS (Listof (Pairof Symbol Any)) -> SExpr)
         (LHS (Listof (Pairof Symbol Any)) -> (U Sexp ((Listof Any) -> Sexp)))
         (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-basic-signature E mk-type mk-code vars)
  ;; E : [BasicSignature ::= (Type "::=" Expression/Binding)]
  (match E
    [(edge-complete 'BasicSignature (list (? edge? T) _ (? edge? E/B)))
     (let-values ([(rhs assoc prec) (parse-expression/binding E/B)])
       (let ([lhs (parse-type T)]
             [formals (get-formals rhs)]
             [vs (make-hash vars)])
         (for ([var formals])
           (hash-set! vs (symbol->string (car var)) (cons #f (cdr var))))
         (let* ([type (mk-type lhs formals)]
                [proc (mk-code lhs formals)]
                [code (cons type proc)]
                [rhs (list (half-rule rhs assoc prec vs code))])
           (list (cons lhs (cons '::= rhs))))))]))

(: parse-scope-signature
   (edge (LHS (Listof (Pairof Symbol Any)) -> SExpr)
         (LHS (Listof (Pairof Symbol Any)) -> (U Sexp ((Listof Any) -> Sexp)))
         (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-scope-signature E mk-type mk-code vars)
  ;; E : [ScopeSignature ::= (Type "::=" Term/Binding+ ScopeDef)]
  (match E
    [(edge-complete 'ScopeSignature
                    (list (? edge? T) _ (? edge? T/B) (? edge? SD)))
     (let* ([lhs (parse-type T)]
            [ts (parse-term/binding+ T/B)]
            [rhs (list (parse-scope-def SD lhs ts mk-type mk-code vars))])
       (list (cons lhs (cons '::= rhs))))]))

(: parse-scope-def
   (edge LHS (Listof Term)
         (LHS (Listof (Pairof Symbol Any)) -> SExpr)
         (LHS (Listof (Pairof Symbol Any)) -> (U Sexp ((Listof Any) -> Sexp)))
         (Listof (Pairof Symbol Any)) -> half-rule))
(define (parse-scope-def E lhs t/bs mk-type mk-code vars)
  ;; E : [ScopeDef ::= ("{" Binding+ ";" Term/Binding+ "}" (? String))]
  (let* ([b (parse-binding+ (second (edge-found E)))]
         [t/bs2 (parse-term/binding+ (fourth (edge-found E)))]
         [formals (append (get-formals t/bs) (get-formals t/bs2))]
         [vs (make-hash vars)]
         [s (if (listn? 6 (edge-found E))
                (list (sixth (edge-found E)))
                '())])
    (for ([var b])
      (let ([p var])
        (hash-set! vs (car p) (cons #f (cdr p)))))
    (let ([type (mk-type lhs formals)]
          [proc (mk-code lhs formals)]
          [rhs `(,@t/bs "{" ,@t/bs2 "}" ,@(map parse-term s))])
      (when (or type proc)
        (for ([var formals])
          (let ([key (symbol->string (car var))])
            (unless (hash-has-key? vs key)
              (hash-set! vs key (cons #f (cdr var)))))))
      (let ([code (cons type proc)])
        (half-rule rhs '⊥ '⊥ vs code)))))

(: parse-scope-rule (edge (Listof (Pairof Symbol Any)) -> (Listof Rule-Spec)))
(define (parse-scope-rule E vars)
  ;; E : [ScopeRule ::= (ScopeSignature ";")]
  (: mk-type (LHS (Listof (Pairof Symbol Any)) -> SExpr))
  (define (mk-type lhs formals) '())
  (: mk-code (LHS (Listof (Pairof Symbol Any)) -> Sexp))
  (define (mk-code lhs formals) '())
  (let ([sig (first (edge-found E))])
    (map (λ: ([rs : Rule-Spec])
           ;; TODO: make this cleaner somehow
           (let ([lhs (car rs)]
                 [terms (map (λ: ([t : (U (Listof Term) half-rule)])
                               (match t
                                 [(half-rule rhs assoc prec vars code)
                                  (half-rule rhs assoc prec vars #f)]
                                 [_ t]))
                             (cddr rs))])
             (cons lhs (cons '::= terms))))
         (parse-scope-signature sig mk-type mk-code vars))))

(: parse-expression/binding (edge -> (Values (Listof Term) Assoc Prec)))
(define (parse-expression/binding E)
  ;; E : [Expression/Binding ::= Term/Binding+ (Term/Binding+ Attr)]
  (let ([ts (parse-term/binding+ (first (edge-found E)))])
    (cond [(list1? (edge-found E))
           (values ts '⊥ '⊥)]
          [(list2? (edge-found E))
           (let ([attr (second (edge-found E))])
             (let-values ([(assoc prec) (parse-attr attr)])
               (values ts assoc prec)))]
          [else (error "parse-expression/binding unexpected:" E)])))

(: parse-term/binding (edge -> Term))
(define (parse-term/binding E)
  ;; E : [Term/Binding ::= Term Binding]
  (let ([t (first (edge-found E))])
    (if (eq? 'Binding (edge-lhs t))
        (parse-binding t)
        (parse-term t))))

(: parse-binding (edge -> Term))
(define (parse-binding E)
  ;; E : [Binding ::= *Name ":" Type]
  (cons (unparse (first (edge-found E)))
        (parse-type (third (edge-found E)))))

(: parse-rule+ (edge -> (Listof Rule-Spec)))
(define (parse-rule+ E)
  ;; E : [Rule+ ::= (Rule (? Rule+))]
  (apply append (parse-rec E (mk-base parse-rule) first (mk-rec second))))

(: parse-term+ (edge -> (Listof Term)))
(define (parse-term+ E)
  ;; E : [Term+ ::= (Term (? Term+))]
  (parse-rec E (mk-base parse-term) first (mk-rec second)))

(: parse-term/binding+ (edge -> (Listof Term)))
(define (parse-term/binding+ E)
  ;; E : [Term/Binding+ ::= (Term/Binding (? Term/Binding+))]
  (parse-rec E (mk-base parse-term/binding) first (mk-rec second)))

(: parse-binding+ (edge -> (Listof Term)))
(define (parse-binding+ E)
  ;; E : [Binding+ ::= (Binding (? Binding+))]
  (parse-rec E (mk-base parse-binding) first (mk-rec second)))

(: parse-binding-list (edge -> (Listof Term)))
(define (parse-binding-list E)
  ;; E : [BindingList ::= (Binding (Binding "," BindingList))]
  (parse-rec E (mk-base parse-binding) first (mk-rec third)))

(: parse-sexp+ (edge -> Sexp))
(define (parse-sexp+ E)
  ;; E : [SExp+ ::= (SExp (? SExp+))] (i.e., SExp ~ Text)
  (let ([ts (vector->list (extract-text E))])
    (with-input-from-string
        (string-join (map token-value ts) " ")
      read)))

(define-syntax-rule (mk-base base)
  (λ: ([e : Edge-Term])
    (base e)))

(define-syntax-rule (mk-rec rec)
  (λ: ([es : (Listof Edge-Term)])
    (rec es)))

(: parse-rec
   (All (T) (edge (Edge-Term -> T) ((Listof Edge-Term) -> Edge-Term)
                  ((Listof Edge-Term) -> edge) -> (Listof T))))
(define (parse-rec E base fst rec)
  ;; for parsing recursive grammar rules, e.g., A ::= B | B A;
  ;;   (define (parse-A E)
  ;;     (parse-rec E parse-B first second))
  (let loop ([E E] [F '()])
    (let ([e (base (fst (edge-found E)))])
      (if (list1? (edge-found E))
          (reverse (cons e F))
          (loop (rec (edge-found E)) (cons e F))))))

(: get-formals ((Listof Term) -> (Listof (Pairof Symbol Any))))
(define (get-formals t/bs)
  (let loop ([bs t/bs] [formals '()])
    (match bs
      ['() (reverse formals)]
      [(list (cons (? string? s) v) _ ...)
       (let ([f (cons (string->symbol s) v)])
         (loop (cdr bs) (cons f formals)))]
      [_ (loop (cdr bs) formals)])))

(: fun-parse? ((HashTable Symbol (Option (Listof Symbol))) ->
               (edge -> Boolean)))
(define (fun-parse? args)
  (λ: ([e : edge])
    (and (edge-complete? e)
         (hash-has-key? args (cdr (edge-src e))))))

(: extract-text (edge -> (Vectorof token)))
(define (extract-text E)
  ;; E : [Text+ ::= (Text (? Text+))]
  (let ([parse-text
         (λ: ([p : Edge-Term])
           (match p
             [(cons (or 'Text 'SExp) (? token? t)) t]))])
    (let loop ([E E] [txt '()])
      (let ([t (parse-text (first (edge-found E)))])
        (if (list1? (edge-found E))
            (list->vector (reverse (cons t txt)))
            (loop (second (edge-found E))
                  (cons t txt)))))))

(: scope-extension? (edge -> Boolean))
(define (scope-extension? E)
  (and (eq? 'Extension (edge-lhs E))
       (eq? 'Scope (edge-lhs (first (edge-found E))))))

(: env-lookup
   (case-> [Env String -> Any]
           [Env String (-> Any) -> Any]))
(define env-lookup
  (let ([lk (λ: ([env : Env] [x : String] [failure-result : (-> Any)])
              (match env
                [(cons (? hash? vars) next)
                 (hash-ref vars x (λ () (env-lookup next x failure-result)))]
                [_ (failure-result)]))])
    (case-lambda:
     [([env : Env] [x : String])
      (lk env x (λ () (error "unbound variable:" x)))]
     [([env : Env] [x : String] [failure-result : (-> Any)])
      (lk env x failure-result)])))

(: make-env (Env (Listof (HashTable LHS Any)) -> Env))
(define (make-env top-env vs)
  (: vars->env ((HashTable LHS Any) -> (HashTable String Any)))
  (define (vars->env vars)
    (let ([new-env (make-hash)])
      (hash-for-each vars
        (λ: ([k : LHS] [v : Any])
          (when (and (string? k) (list? v))
            (for-each
             (λ (p)
               (when (pair? p)
                 (let ([s (car p)] [lhs (cdr p)])
                   (when (and (string? s) (rule-lhs? lhs)
                              (not (string? lhs)))
                     (let ([val (or (lookup vars lhs) lhs)])
                       (hash-set! new-env s val)))))) v))))
      new-env))
  (let ([new-env (make-hash)])
    (for ([env (map vars->env vs)])
      (hash-for-each env
       (λ: ([k : String] [v : Any])
         (if (hash-has-key? new-env k)
             (let ([new-v (hash-ref new-env k)])
               (unless (member v new-v)
                 (hash-set! new-env k (cons v new-v))))
             (hash-set! new-env k (list v))))))
    (cons new-env top-env)))

(: next-state/declare
   ((Option parser-state) edge grammar grammar Env Boolean ->
    (Option parser-state)))
(define (next-state/declare state body G H env aux?)
  (: mk-update? (Integer (Setof LHS) -> (edge -> Boolean)))
  (define (mk-update? j types)
    (λ: ([e : edge])
      (and (= (edge-start e) j)
           (= (edge-end e) j)
           (edge-incomplete? e)
           (set-member? types (edge-lhs e)))))
  (: add! (edge Chart -> Void))
  (define (add! e chart)
    (for ([c (list (car chart) (cdr chart))])
      (let* ([j (edge-start e)]
             [lst (vector-ref c j)])
        (unless (member e lst)
          (vector-set! c j (cons e lst))))))
  (match state
    [(parser-state c1 c1-dep param-spec counts stats es aux)
     (let* ([j (- (vector-length (cdr c1)) 1)]
            [ts (list->set (map rule-lhs (grammar-lexicon H)))]
            [es (chart-filter (mk-update? j ts) c1 j 'start)]
            [ls (list->set (map edge-lhs es))])
       (for: ([lhs : LHS (set->list ls)])
         (for: ([r : rule (rewrites-for lhs H)])
           (let ([A (rule-lhs r)]
                 [x (rule-rhs r)]
                 [src (rule-src r)]
                 [vars (make-hash)])
             (add! (edge j j A '() '() x '⊥ '⊥ vars #f src) c1))))
       (next-state state body G env aux?))]
    [_ #f]))

(: state/undeclare! (parser-state grammar -> Void))
(define (state/undeclare! state G)
  (: mk-update? (Integer (Setof LHS) -> (edge -> Boolean)))
  (define (mk-update? j types)
    (λ: ([e : edge])
      (and (= (edge-start e) j)
           (= (edge-end e) j)
           (edge-incomplete? e)
           (set-member? types (edge-lhs e)))))
  (: remove! (edge Chart -> Void))
  (define (remove! e chart)
    (for ([c (list (car chart) (cdr chart))])
      (let* ([j (edge-start e)]
             [lst (vector-ref c j)])
        (vector-set! c j (remove e lst)))))
  (match state
    [(parser-state c1 c1-dep param-spec counts stats es aux)
     (let* ([j (- (vector-length (cdr c1)) 1)]
            [ts (list->set (map rule-lhs (grammar-lexicon G)))]
            [es (chart-filter (mk-update? j ts) c1 j 'start)]
            [ls (list->set (map edge-lhs es))])
       (for: ([lhs : LHS (set->list ls)])
         (for: ([r : rule (rewrites-for lhs G)])
           (let ([A (rule-lhs r)]
                 [x (rule-rhs r)]
                 [src (rule-src r)]
                 [vars (make-hash)])
             (remove! (edge j j A '() '() x '⊥ '⊥ vars #f src) c1)))))]))
  
(: next-state
   ((Option parser-state) edge grammar Env Boolean -> (Option parser-state)))
(define (next-state state body G env aux?)
  (and state
       (let ([words (extract-text (first (edge-found body)))])
         (next-state-tokens state words G env aux?))))

(: next-state-tokens
   ((Option parser-state) (Vectorof token) grammar Env Boolean ->
    (Option parser-state)))
(define (next-state-tokens state tokens G env aux?)
  (match state
    [(parser-state c1 c1-dep param-spec counts stats es aux)
     (let* ([end (- (vector-length (cdr c1)) 1)]
            [c2 (resume-chart tokens G c1)]
            [aa (resume-agenda tokens G end env aux?)]
            [agenda (car aa)]
            [aux (cdr aa)])
       (parser-state c2 c1-dep param-spec counts stats agenda aux))]
    [_ #f]))

(: close-state (parser-state grammar -> (Listof Sexp)))
(define (close-state state G)
  (match state
    [(parser-state c1 c1-dep param-spec counts stats es aux)
     (map (λ: ([e : edge]) (edge->ast e *edge->ast-terminals?*))
          (chart->parses c1 (grammar-start G)))]))

(: resume-agenda
   ((Vectorof token) grammar Integer Env Boolean ->
    (Pairof (Listof edge) (Listof (Listof edge)))))
(define (resume-agenda tokens G start env aux?)
  (let loop ([agenda '()]
             [aux '()]
             [i 0])
    (if (= i (vector-length tokens))
        (cons (reverse agenda)
              (list (if aux? (reverse aux) '())))
        (let* ([t (vector-ref tokens i)] [w (token-value t)]
               [rn (cons (grammar-id G) (string->symbol w))]
               [j (+ i start)] [k (+ j 1)] [lhs w] [rhs (list t)]
               [use-aux? #f]
               [es (sort
                    (map
                     (λ (new-lhs)
                       (set! use-aux? #t)
                       (let ([lhs new-lhs]
                             [rhs (list (cons w t))]
                             [lr '()]
                             [vars (make-hash)])
                         (edge j k lhs lr rhs lr '⊥ '⊥ vars #f rn)))
                     (env-lookup env w null))
                    (λ: ([e : edge] [f : edge])
                      (more-specific? (edge-lhs e) (edge-lhs f) G)))]
               [f (edge j k w '() (list t) '() '⊥ '⊥ (make-hash) #f rn)])
          (if use-aux?
              (let ([as (append aux (cons f (cdr es)))])
                (loop (cons (car es) agenda) as (+ i 1)))
              (loop (cons f agenda) aux (+ i 1)))))))

(: resume-chart ((Vectorof token) grammar Chart -> Chart))
(define (resume-chart tokens G chart)
  (let ([c (bu-initial-chart (vector-drop-right tokens 1) G)])
    (cons (vector-append (car chart) (car c))
          (vector-append (cdr chart) (cdr c)))))

(: test (String -> Void))
(define (test filename)
  (printf "island:   ")
  (printf " trees=~a~n" (length (parse-file/island filename #f #t)))
  (printf "bu-earley:")
  (printf " trees=~a~n" (length (parse-file/bu-earley filename #f #t)))
  (printf "td-earley:")
  (printf " trees=~a~n" (length (parse-file/td-earley filename #f #t)))
  (void))
