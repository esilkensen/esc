;;;;;; lexer.rkt - Lexical Analyzer module.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 3 Sep 2012

#lang typed/racket/no-check

(require "utils.rkt")

(provide 
 read-token
 peek-token
 match-token
 tokenize-file
 tokenize-string
 tokenize-input
 token
 token?
 token-name
 token-value
 token-position
 token-column
 token-line
 token-source
 token->list)

(define-type Location (Pairof (Option Integer) (Option Integer)))

(struct: token
  ([name : Symbol] [value : String] [position : Location] [source : String])
  #:transparent)

(: tokenize-file : (String -> (Vectorof token)))
(define (tokenize-file filename)
  (with-input-from-file filename
    (λ () (tokenize-input (current-input-port) filename))))

(: tokenize-string : (String String -> (Vectorof token)))
(define (tokenize-string string src)
  (with-input-from-string string
    (λ () (tokenize-input (current-input-port) src))))

(: tokenize-input (Input-Port String -> (Vectorof token)))
(define (tokenize-input in src)
  (port-count-lines! in)
  (let: loop : (Vectorof token) ([ts : (Listof token) '()])
    (let ([tok (read-token in src)])
      (if tok
          (let ([val (if (string-token? tok)
                         (format "~a" (token-value tok))
                         (token-value tok))])
            (loop (cons (token (token-name tok) val
                               (token-position tok) (token-source tok))
                        ts)))
          (list->vector (reverse ts))))))

(: *peek* (Listof (Option token)))
(define *peek* '())

(: read-token (Input-Port String -> (Option token)))
(define (read-token in src)
  (if (null? *peek*)
      (<token> in src)
      (pop! *peek*)))

(: peek-token (Input-Port String -> (Option token)))
(define (peek-token in src)
  (if (null? *peek*)
      (let ([token (read-token in src)])
        (set! *peek* (if token (list token) '()))
        token)
      (pop! *peek*)))

(: match-token
   (case-> [Regexp -> (String -> (Option String))]
           [Regexp (Listof String) -> (String -> (Option String))]))
(define match-token
  (let ([mt (λ: ([regexp : Regexp] [except : (Listof String)])
              (λ: ([str : String])
                (and (regexp-match regexp str)
                     (not (member str except))
                     str)))])
    (case-lambda:
     [([regexp : Regexp])
      (mt regexp '())]
     [([regexp : Regexp] [except : (Listof String)])
      (mt regexp except)])))

(: <token> (Input-Port String -> (Option token)))
(define (<token> in src)
  ;; the starting state for every token. #f is returned at the end of
  ;; input, a token object is returned if possible, and an error is
  ;; raised otherwise
  (let* ([next-loc (next-location in)] [next-char (peek-char in)])
    (cond [(eof-object? next-char) #f]
          [(char-whitespace? next-char)
           (begin (read-char in)
                  (<token> in src))]
          [(char=? #\/ next-char)
           (<line-comment> (list (read-char/no-eof in)) next-loc in src)]
          [(char-identifier-initial? next-char)
           (<identifier> (list (read-char/no-eof in)) next-loc in src)]
          [(char=? #\# next-char)
           (<sharp> (list (read-char/no-eof in)) next-loc in src)]
          [(or (char-peculiar? next-char)
               (char-numeric? next-char))
           (<number> '() next-loc in src)]
          [(char=? #\" next-char)
           (<string> (list (read-char/no-eof in)) next-loc in src)]
          [(char=? #\, next-char)
           (<comma> (list (read-char/no-eof in)) next-loc in src)]
          [(char=? #\. next-char)
           (<dot> (list (read-char/no-eof in)) next-loc in src)]
          [(char=? #\: next-char)
           (<colon> (list (read-char/no-eof in)) next-loc in src)]
          [(char=? #\\ next-char)
           (<backslash> (list (read-char/no-eof in)) next-loc in src)]
          [else
           (let ([next-char (read-char/no-eof in)])
             (cond [(char=? #\( next-char)
                    (token 'LEFT-PAREN "(" next-loc src)]
                   [(char=? #\) next-char)
                    (token 'RIGHT-PAREN ")" next-loc src)]
                   [(char=? #\' next-char)
                    (token 'SINGLE-QUOTE "'" next-loc src)]
                   [(char=? #\` next-char)
                    (token 'BACKQUOTE "`" next-loc src)]
                   [(char=? #\[ next-char)
                    (token 'LEFT-BRACKET "[" next-loc src)]
                   [(char=? #\] next-char)
                    (token 'RIGHT-BRACKET "]" next-loc src)]
                   [(char=? #\{ next-char)
                    (token 'LEFT-BRACE "{" next-loc src)]
                   [(char=? #\} next-char)
                    (token 'RIGHT-BRACE "}" next-loc src)]
                   [(char=? #\; next-char)
                    (token 'SEMICOLON ";" next-loc src)]
                   [(char=? #\| next-char)
                    (token 'BAR "|" next-loc src)]
                   [else
                    (syntax-error next-loc next-char)]))])))

(: <line-comment> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<line-comment> chars location in src)
  ;; a line comment begins with ``//'' and extends until the end of the
  ;; line. this procedure assumes that the first ``/'' has already been
  ;; matched
  (let ([next-char (peek-char/no-eof in)])
    (cond [(char=? #\/ next-char)
           (read-line in)
           (<token> in src)]
          [else (<identifier> chars location in src)])))

(: <identifier> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<identifier> chars location in src)
  ;; an identifier begins with an identifier-initial and extends (with 
  ;; identifier-subsequent) until a delimiter. it is an error for an
  ;; identifier not to be terminated by a delimiter (or the end of input).
  ;; this procedure assumes that an identifier-initial has already been
  ;; matched
  (let ([next-char (peek-char/no-eof in)])
    (cond [(char=? #\: next-char)
           (let* ([this-char (read-char/no-eof in)]
                  [next-char (peek-char/no-eof in)])
             (if (and (char-delimiter? next-char)
                      (not (char=? #\: next-char))
                      (not (char=? #\( next-char)))
                 (let* ([chars (cons this-char chars)]
                        [value (list->string (reverse chars))])
                   (push! (<token> in src) *peek*)
                   (token 'IDENTIFIER value location src))
                 (let* ([value (list->string (reverse chars))]
                        [chars (cons this-char chars)])
                   (push! (<colon> chars location in src) *peek*)
                   (token 'IDENTIFIER value location src))))]
          [(char-identifier-subsequent? next-char)
           (<identifier> (cons (read-char/no-eof in) chars) location in src)]
          [(or (char-delimiter? next-char)
               (char=? #\. next-char))
           (let ([value (list->string (reverse chars))])
             (token 'IDENTIFIER value location src))]
          [else
           (syntax-error location (list->string (reverse chars)))])))

(: <sharp> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<sharp> chars location in src)
  ;; a sharp ``#'' is the starting state for several tokens. this
  ;; procedure assumes that the sharp has already been matched, and then
  ;; moves to the next state.
  ;; <sharp> ::= <vector> | <boolean> | <character> | <number> | <regex>
  ;; ff none of these alternatives match, an <identifier> is assumed
  (let ([next-char (peek-char/no-eof in)])
    (cond [(char=? #\( next-char)
           (<vector> (cons (read-char/no-eof in) chars) location in src)]
          [(char-boolean? next-char)
           (<boolean> (cons (read-char/no-eof in) chars) location in src)]
          [(char=? #\\ next-char)
           (<character> (cons (read-char/no-eof in) chars) location in src)]
          [(or (char=? #\r next-char)
               (char=? #\p next-char))
           (<regex> chars location in src)]
          [(or (char-radix? next-char)
               (char-exactness? next-char))
           (<number> chars location in src)]
          [else (<identifier> chars location in src)])))

(: <vector> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<vector> chars location in src)
  ;; a vector ``#('' represents the start of a literal vector
  ;; definition. this procedure assumes that the vector token has
  ;; already been matched
  (token 'VECTOR "#(" location src))

(: <boolean> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<boolean> chars location in src)
  ;; a boolean ``#t'' or ``#f'' represents a literal boolean value. this
  ;; procedure assumes that the boolean token has already been matched,
  ;; and verifies that it terminates with a delimiter. it is an error
  ;; for a boolean not to do so
  (let ([next-char (peek-char in)])
    (if (char-delimiter? next-char)
        (token 'BOOLEAN (list->string (reverse chars)) location src)
        (<identifier> chars location in src))))

(: <character> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<character> chars location in src)
  ;; a character has either the form ``#\<character>'' or
  ;; ``#\<character name>''. this procedure assumes that the ``#\'' has
  ;; already been matched, and verifies that the rest of the token
  ;; terminates with a delimiter. it is an error for a character not to
  ;; do so.
  (let ([next-char (peek-char/no-eof in)])
    (cond [(char=? #\s (char-downcase next-char))
           (let ([chars (cons (read-char/no-eof in) chars)]
                 [char-space (string->list "pace")])
             (if (char-delimiter? (peek-char in))
                 (token 'CHARACTER (list->string (reverse chars)) location src)
                 (<character-name> chars char-space location in src)))]
          [(char=? #\n (char-downcase next-char))
           (let ([chars (cons (read-char/no-eof in) chars)]
                 [char-newline (string->list "ewline")])
             (if (char-delimiter? (peek-char in))
                 (token 'CHARACTER (list->string (reverse chars)) location src)
                 (<character-name> chars char-newline location in src)))]
          [else
           (let* ([chars (cons (read-char/no-eof in) chars)]
                  [value (list->string (reverse chars))]
                  [next-char (peek-char in)])
             (if (char-delimiter? next-char)
                 (token 'CHARACTER value location src)
                 (syntax-error location value)))])))

(: <character-name>
   ((Listof Char) (Listof Char) Location Input-Port String -> (Option token)))
(define (<character-name> chars match location in src)
  ;; a character name is a special character constant (e.g. #\newline).
  ;; this procedure assumes that the first character of the name has
  ;; already been matched, matches until the match parameter is the null
  ;; list, and then verifies that character name terminates with a
  ;; delimiter. it is an error not to do so.
  (let loop ([chars chars] [match match])
    (let ([next-char (peek-char in)])
      (cond [(and (char-delimiter? next-char)
                  (null? match))
             (token 'CHARACTER (list->string (reverse chars)) location src)]
            [(and (not (null? match))
                  (not (eof-object? (car match)))
                  (not (eof-object? next-char))
                  (char=? (char-downcase next-char) (car match)))
             (loop (cons (read-char/no-eof in) chars) (cdr match))]
            [else (syntax-error location 
                                (list->string (reverse chars)))]))))

(: <regex> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<regex> chars location in src)
  ;; a regex is the sequence #[rp]x<string>. this procedure assumes that
  ;; the opening ``#'' has already been matched
  (let ([next-char (peek-char in)])
    (cond
     [(eof-object? next-char)
      (syntax-error location (list->string (reverse chars)))]
     [(or (char=? #\r next-char)
          (char=? #\p next-char))
      (let* ([regex-mode (read-char/no-eof in)]
             [chars (cons regex-mode chars)]
             [next-char (peek-char in)])
        (if (and (char? next-char) (char=? #\x next-char))
            (let* ([chars (cons (read-char/no-eof in) chars)]
                   [next-char (peek-char in)])
              (if (and (char? next-char) (char=? #\" next-char))
                  (let* ([chars (begin (read-char in) (cddr chars))]
                         [tok (<string> chars location in src)]
                         [val (format "#~ax~a" regex-mode (token-value tok))])
                    (token 'REGEX val (token-position tok) src))
                  (<identifier> chars location in src)))
            (<identifier> chars location in src)))]
     [else
      (<identifier> chars location in src)])))

(: <string> ((Listof Char) Location Input-Port String -> token))
(define (<string> chars location in src)
  ;; a string is a sequence of characters (or escaped characters)
  ;; terminating with a ``"''. this procedure assumes that the opening
  ;; ``"'' has already been matched. tt is an error for a string not to
  ;; terminate with a ``"''.
  (let ([next-char (peek-char in)])
    (cond [(eof-object? next-char)
           (syntax-error location (list->string (reverse chars)))]
          [(char-string-element? next-char)
           (<string> (cons (read-char/no-eof in) chars) location in src)]
          [(char=? #\\ next-char)
           (let* ([this-char (read-char in)]
                  [next-char (peek-char in)])
             (if (char-escape? next-char)
                 (let ([esc (char->escape (read-char/no-eof in))])
                   (<string> (cons esc chars) location in src))
                 (syntax-error location (list->string (reverse chars)))))]
          [(char=? #\" next-char)
           (let* ([next-char (read-char in)]
                  [value (list->string (cdr (reverse chars)))])
             (token 'STRING (format "\"~a\"" value) location src))]
          [else
           (syntax-error location (list->string (reverse chars)))])))

(: <number> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<number> chars location in src)
  ;; a number token is made up of prefix and complex subtokens. in
  ;; general, this procedure assumes that the start of a character has
  ;; been detected, but not matched (i.e. consumed) - other than an
  ;; initial ``#'' character
  (let ([prefix (match-prefix chars in)])
    (<complex> (car prefix) (cdr prefix) location in src)))

(: match-prefix
   ((Listof Char) Input-Port -> (Pairof (Listof Char) (Char -> Boolean))))
(define (match-prefix chars in)
  ;; attempts to match the prefix of a number. the return value is a
  ;; pair where the first element is the list of characters so far
  ;; matched, and the second element is a predicate function that
  ;; returns whether or not a given character is a valid digit for a
  ;; number with the detected radix. this procedure assumes that at most
  ;; the initial ``#'' marking a radix or exactness has been matched
  (let ([radix (match-radix chars in)])
    (if (not radix)
        (if (char-exactness? (peek-char in))
            (let ([chars (cons (read-char/no-eof in) chars)])
              (or (match-#-<R> match-radix chars in)
                  (cons chars char-numeric?)))
            (cons chars char-numeric?))
        (let ([exactness (match-#-<R> match-exactness (car radix) in)])
          (if exactness
              (cons exactness (cdr radix))
              radix)))))

(: match-radix
   ((Listof Char) Input-Port ->
    (Option (Pairof (Listof Char) (Char -> Boolean)))))
(define (match-radix chars in)
  ;; attempts to match the radix of a number. the return value is a pair
  ;; where the first element is the list of characters so far matched,
  ;; and the second element is a predicate function that returns whether
  ;; or not a given character is a valid digit for a number with the
  ;; detected radix. if the radix is empty, it is assumed to be decimal.
  ;; if the radix is invalid, #f is returned. this procedure assumes
  ;; that the ``#'' (if present) has already been matched
  (let ([next-char (peek-char in)])
    (if (and (char? next-char) (char-radix? next-char))
        (cons (cons (read-char/no-eof in) chars)
              (char-radix->digits? next-char))
        #f)))
              
(: match-exactness
   ((Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-exactness chars in)
  ;; attempts to match the exactness of a number. if an invalid
  ;; exactness is detected, #f is returned. this procedure assumes that
  ;; the ``#'' has already been matched
  (let ([next-char (peek-char in)])
    (if (char-exactness? next-char)
        (cons (read-char/no-eof in) chars)
        #f)))

(: <complex>
   ((Listof Char) (Char -> Boolean) Location Input-Port String ->
    (Option token)))
(define (<complex> chars pred? location in src)
  ;; the complex portion of a number is documented in Ch. 4.2.1 of the
  ;; R^6RS sepcification. pred? should be a predicate procedure for
  ;; testing whether or not a given character is valid for the number
  ;; being matched. we follow many implementations in their choice to
  ;; not follow the specification: a failed match of a number will
  ;; usually transfer to the <identifier> state
  (let* ([sign (match-sign chars in)]
         [next-char (peek-char in)])
    (if (and (char? next-char) (char=? #\i (char-downcase next-char)))
        (<i> sign location in src)
        (let* ([ureal (match-ureal-+ pred? sign in)]
               [next-char (peek-char in)])
          (cond [(not ureal)
                 (<identifier> sign location in src)]
                [(char-delimiter? next-char)
                 (token 'NUMBER (list->string (reverse ureal)) location src)]
                [(and (char? next-char) (char=? #\@ next-char))
                 (let ([chars (cons (read-char/no-eof in) ureal)])
                   (<real> pred? chars location in src))]
                [(char-sign? next-char)
                 (let* ([chars (cons (read-char/no-eof in) ureal)]
                        [digits (match-ureal-* pred? chars in)])
                   (and digits (<i> digits location in src)))]
                [else
                 (<i> ureal location in src)])))))
         
(: <i> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<i> chars location in src)
  ;; matches the imaginary ``i'' portion of a number, where the ``i'' is
  ;; meant to terminate the number. a move to the identifier state is
  ;; possible
  (let ([next-char (peek-char in)])
    (if (and (char? next-char) (char=? #\i (char-downcase next-char)))
        (let* ([chars (cons (read-char/no-eof in) chars)]
               [next-char (peek-char in)])
          (if (char-delimiter? next-char)
              (token 'NUMBER (list->string (reverse chars)) location src)
              (<identifier> chars location in src)))
        (<identifier> chars location in src))))

(: <real>
   ((Char -> Boolean) (Listof Char) Location Input-Port String ->
    (Option token)))
(define (<real> pred? chars location in src)
  ;; matches a real number. pred? should be a predicate procedure for
  ;; testing whether or not a given character is valid for the number
  ;; being matched. this procedure assumes that at most the first digit
  ;; has already been matched. it attempts to match a sign before
  ;; transfering to the ureal state.
  (<ureal> pred? (match-sign chars in) location in src))

(: <ureal>
   ((Char -> Boolean) (Listof Char) Location Input-Port String ->
    (Option token)))
(define (<ureal> pred? chars location in src)
  ;; matches an unsigned real number. pred? should be a predicate
  ;; procedure for testing whether or not a given character is valid for
  ;; the number being matched. this procedure assumes that at most the
  ;; first digit has already been matched
  (let* ([chars (match-uinteger-* pred? chars in)]
         [next-char (peek-char in)])
    (cond [(char-delimiter? next-char)
           (token 'NUMBER (list->string (reverse chars)) location src)]
          [(and (char? next-char) (char=? #\/ next-char))
           (let ([chars (cons (read-char/no-eof in) chars)])
             (<uinteger> pred? chars location in src))]
          [(and (char? next-char)
                (char=? #\. next-char)     ; pred? must be char-numeric?
                (eq? pred? char-numeric?)) ; in order to move to <decimal>
           (<decimal> (cons (read-char/no-eof in) chars) location in src)]
          [(and (char? next-char)
                (char-exponent-marker? next-char)
                (eq? pred? char-numeric?))
           (<suffix> chars location in src)]
          [else
           (syntax-error location (list->string (reverse chars)))])))

(: match-ureal-+
   ((Char -> Boolean) (Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-ureal-+ pred? chars in)
  ;; attempts to match an unsigned real number, requiring that at least
  ;; one digit is matched immediately. pred? should be a predicate
  ;; procedure for testing whether or not a given character is valid for
  ;; the number being matched. #f is returned on error
  (match-ureal match-uinteger-+ pred? chars in))

(: match-ureal-*
   ((Char -> Boolean) (Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-ureal-* pred? chars in)
  ;; attempts to match an unsigned real number. pred? should be a
  ;; predicate procedure for testing whether or not a given character is
  ;; valid for the number being matched. #f is returned on error
  (match-ureal match-uinteger-* pred? chars in))

(: match-ureal
   (((Char -> Boolean) (Listof Char) Input-Port -> (Option (Listof Char)))
    (Char -> Boolean) (Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-ureal match pred? chars in)
  ;; generic procedure that attempts to match an unsigned real number.
  ;; match should be a procedure to apply immediately to the chars.
  ;; pred? should be a procedure for testing whether or not a given
  ;; character is valid for the number being matched. #f is returned on
  ;; error
  (let* ([chars (match pred? chars in)]
         [next-char (peek-char in)])
    (cond [(not chars) #f]
          [(char-delimiter? next-char) chars]
          [(and (char? next-char) (char=? #\/ next-char))
           (match-uinteger-+ pred? (cons (read-char/no-eof in) chars) in)]
          [(and (char? next-char)
                (char=? #\. next-char)
                (eq? pred? char-numeric?))
           (match-decimal (cons (read-char/no-eof in) chars) in)]
          [(and (char? next-char)
                (char-exponent-marker? next-char)
                (eq? pred? char-numeric?))
           (match-suffix chars in)]
          [else chars])))

(: <uinteger>
   ((Char -> Boolean) (Listof Char) Location Input-Port String ->
    (Option token)))
(define (<uinteger> pred? chars location in src)
  ;; matches an unsigned integer number. pred? should be a predicate
  ;; procedure for testing whether or not a given character is valid for
  ;; the number being mached. it is an error for a number to not be
  ;; terminated with a delimiter
  (let* ([digits (match-uinteger-+ pred? chars in)]
         [next-char (peek-char in)])
    (if digits
        (if (char-delimiter? next-char)
            (token 'NUMBER (list->string (reverse digits)) location src)
            (syntax-error location (list->string (reverse digits))))
        (syntax-error location (list->string (reverse chars))))))

(: match-uinteger-+
   ((Char -> Boolean) (Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-uinteger-+ pred? chars in)
  ;; attempts to match an unsigned integer number, requiring that at
  ;; least one digit is matched immediately. pred? should be a predicate
  ;; procedure for testing whether or not a given character is valid for
  ;; the number being matched. #f is returned on error
  (let ([digits (match-+ pred? chars in)])
    (if digits digits #f)))

(: match-uinteger-*
   ((Char -> Boolean) (Listof Char) Input-Port -> (Listof Char)))
(define (match-uinteger-* pred? chars in)
  ;; attempts to match an unsigned integer number. pred? should be a
  ;; predicate procedure for testing whether or not a given character is
  ;; valid for the number being matched
  (let ([digits (match-* pred? chars in)])
    (if digits digits chars)))

(: <decimal> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<decimal> chars location in src)
  ;; a decimal number has an optional fractional component and ends with
  ;; an optional exponent. this procedure assumes that at most the first
  ;; character has been matched. ot is an error for a decimal number to
  ;; not terminate with a delimiter
  (let* ([digits (match-decimal chars in)]
         [next-char (peek-char in)])
    (if (and digits
             (char-delimiter? next-char))
        (token 'NUMBER (list->string (reverse digits)) location src)
        (syntax-error location (list->string (reverse chars))))))

(: match-decimal ((Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-decimal chars in)
  ;; attempts to match a decimal number. #f may be returned on error
  (if (= (length chars) 1)
      (let ([chars (match-uinteger-+ char-numeric? chars in)])
        (and chars (match-suffix chars in) ))
      (match-suffix (match-uinteger-* char-numeric? chars in) in)))

(: match-sign ((Listof Char) Input-Port -> (Listof Char)))
(define (match-sign chars in)
  ;; attempts to match a sign (``+'' or ``-''). the characters matched
  ;; are returned whether or not a sign is actually matched (i.e. no #f)
  (let ([next-char (peek-char in)])
    (if (and (char? next-char)
             (or (char=? #\+ next-char)
                 (char=? #\- next-char)))
        (cons (read-char/no-eof in) chars)
        chars)))

(: <suffix> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<suffix> chars location in src)
  ;; the suffix is the end of a decimal number. it may consist of an
  ;; exponent. it is an error for a number to not be terminated with a
  ;; delimiter. this procedure assumes that everything up until the
  ;; start of the suffix has been matched
  (let* ([digits (match-suffix chars in)]
         [next-char (peek-char in)])
    (if (and digits (char-delimiter? next-char))
        (token 'NUMBER (list->string (reverse digits)) location src)
        (syntax-error location (list->string (reverse chars))))))

(: match-suffix
   ((Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-suffix chars in)
  ;; attempts to match the suffix of a decimal number. because a suffix
  ;; can be be empty, the original matched chars are returned if it is
  ;; empty (i.e. no #f). this procedure assumes that everything up until
  ;; the start of the suffix has been matched
  (let ([next-char (peek-char in)])
    (cond [(char-delimiter? next-char) chars]
          [(char-exponent-marker? next-char)
           (set! chars (cons (read-char/no-eof in) chars))
           (set! chars (match-sign chars in))
           (match-+ char-numeric? chars in)]
          [else chars])))

(: <comma> ((Listof Char) Location Input-Port String -> token))
(define (<comma> chars location in src)
  ;; a comma may either be itself a token, or combine with the ``@''
  ;; symbol to make the COMMA-AT token. the comma token doesn't need to
  ;; be explicitly terminated with a delimiter
  (let ([next-char (peek-char in)])
    (if (and (char? next-char) (char=? #\@ next-char))
        (begin (read-char in)
               (token 'COMMA-AT ",@" location src))
        (token 'COMMA "," location src))))

(: <dot> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<dot> chars location in src)
  ;; a dot may itself be a token or it might be the start of a decimal
  ;; number
  (let ([next-char (peek-char in)])
    (cond [(and (= (length chars) 1)
                (char? next-char)
                (char-numeric? next-char))
           (<decimal> (cons (read-char/no-eof in) chars) location in src)]
          [else (token 'DOT "." location src)])))

(: <colon> ((Listof Char) Location Input-Port String -> (Option token)))
(define (<colon> chars location in src)
  ;; a colon may itself be a token or it might be the start of a ::=
  (let ([next-char (peek-char in)])
    (cond [(and (char? next-char) (char=? #\: next-char))
           (let* ([this-char (read-char/no-eof in)]
                  [next-char (peek-char in)])
             (if (and (char? next-char) (char=? #\= next-char))
                 (begin (read-char in)
                        (token 'IDENTIFIER "::=" location src))
                 (<identifier> (cons this-char chars) location in src)))]
          [else (token 'COLON ":" location src)])))

(: <backslash> ((Listof Char) Location Input-Port String -> token))
(define (<backslash> chars location in src)
  (let ([chars (reverse (cons (read-char/no-eof in) chars))])
    (token 'BACKSLASH (list->string chars) location src)))

(: match-+
   ((Char -> Boolean) (Listof Char) Input-Port -> (Option (Listof Char))))
(define (match-+ pred? chars in)
  ;; a generic procedure that matches ``one or more'' characters
  ;; satisfying the predicate procedure ``pred?''. if no characters
  ;; match, #f is returned
  (and (pred? (peek-char/no-eof in))
       (match-* pred? (cons (read-char/no-eof in) chars) in)))

(: match-* ((Char -> Boolean) (Listof Char) Input-Port -> (Listof Char)))
(define (match-* pred? chars in)
  ;; a generic procedure that matches ``zero or more'' characters
  ;; satisfying the predicate procedure ``pred?''. if no characters
  ;; match, chars is returned
  (let loop ([chars chars])
    (if (pred? (peek-char/no-eof in))
        (loop (cons (read-char/no-eof in) chars))
        chars)))

(: match-#-<R>
   (All (T) (((Listof Char) Input-Port -> T) (Listof Char) Input-Port ->
             (Option T))))
(define (match-#-<R> <R> chars in)
  ;; a generic procedure that matches an opening ``#' and then calls the
  ;; specified procedure <R> with the characters so far matched and the
  ;; input port. if the ``#'' match is unsuccessfull, #f is returned
  (and (char=? #\# (peek-char/no-eof in))
       (<R> (cons (read-char/no-eof in) chars) in)))

(: char-delimiter? ((U EOF Char) -> Boolean))
(define (char-delimiter? char)
  (or (eof-object? char)
      (char-whitespace? char)
      (char=? #\( char)
      (char=? #\) char)
      (char=? #\' char)
      (char=? #\` char)
      (char=? #\, char)
      (char=? #\[ char)
      (char=? #\] char)
      (char=? #\{ char)
      (char=? #\} char)
      (char=? #\" char)
      (char=? #\: char)
      (char=? #\; char)
      (char=? #\| char)
      (char=? #\# char)
      (char=? #\\ char)))

(: char-identifier-initial? (Any -> Boolean))
(define (char-identifier-initial? char)
  (and (char? char)
       (not (char-delimiter? char))
       (case char
         [(#\! #\$ #\% #\& #\* #\/ #\< #\= #\> #\? #\^ #\_ #\~) #t]
         [else (or (char-alphabetic? char)
                   (char-symbolic? char))])))

(: char-identifier-subsequent? (Any -> Boolean))
(define (char-identifier-subsequent? char)
  (and (char? char)
       (or (char-identifier-initial? char)
           (char-numeric? char)
           (char=? #\+ char)
           (char=? #\- char)
           (char=? #\@ char))))

(: char-boolean? (Any -> Boolean))
(define (char-boolean? char)
  (and (char? char)
       (let ([char (char-downcase char)])
         (or (char=? #\t char)
             (char=? #\f char)))))

(: char-string-element? (Any -> Boolean))
(define (char-string-element? char)
  (and (char? char)
       (not (char=? #\" char))
       (not (char=? #\\ char))))

(: char-peculiar? (Any -> Boolean))
(define (char-peculiar? char)
  (char-sign? char))

(: char-escape? (Any -> Boolean))
(define (char-escape? char)
  (and (char? char)
       (case (char-downcase char)
         [(#\b #\t #\n #\v #\r #\" #\\) #t]
         [else #f])))

(: char->escape (Char -> Char))
(define (char->escape char)
  (case (char-downcase char)
    [(#\b) #\backspace]
    [(#\t) #\tab]
    [(#\n) #\newline]
    [(#\r) #\return]
    [(#\v) #\vtab]
    [(#\" #\\) char]
    [else (error "unexpected escape")]))

(: char-exactness? (Any -> Boolean))
(define (char-exactness? char)
  (and (char? char)
       (case (char-downcase char)
         [(#\i #\e) #t]
         [else #f])))

(: char-numeric-2? (Char -> Boolean))
(define (char-numeric-2? char)
  (or (char=? #\0 char)
      (char=? #\1 char)))

(: char-numeric-8? (Char -> Boolean))
(define (char-numeric-8? char)
  (let ([ord (char->integer char)])
    (and (>= ord #x30)
         (<= ord #x37))))

(: char-numeric-16? (Char -> Boolean))
(define (char-numeric-16? char)
  (or (char-numeric? char)
      (let ([ord (char->integer (char-downcase char))])
        (and (>= ord #x61)
             (<= ord #x66)))))

(: char-radix? (Char -> Boolean))
(define (char-radix? char)
  (case (char-downcase char)
    [(#\b #\d #\o #\x) #t]
    [else #f]))

(: char-exponent-marker? ((U EOF Char) -> Boolean))
(define (char-exponent-marker? char)
  (and (char? char)
       (case (char-downcase char)
         [(#\e #\s #\f #\d #\l) #t]
         [else #f])))

(: char-radix->digits? (Char -> (Char -> Boolean)))
(define (char-radix->digits? radix)
  (case (char-downcase radix)
    [(#\b) char-numeric-2?]
    [(#\d) char-numeric?]
    [(#\o) char-numeric-8?]
    [(#\x) char-numeric-16?]
    [else (λ (x) #f)]))

(: radix->digits? (Char -> (Option (Char -> Boolean))))
(define (radix->digits? radix)
  (case radix
    [(2) char-numeric-2?]
    [(10) char-numeric?]
    [(8) char-numeric-8?]
    [(16) char-numeric-16?]
    [else (λ (x) #f)]))

(: char-sign? (Any -> Boolean))
(define (char-sign? char)
  (and (char? char)
       (or (char=? #\+ char)
           (char=? #\- char))))

(: identifier-token? (Any -> Boolean))
(define (identifier-token? token)
  (token-type? token 'IDENTIFIER))

(: boolean-token? (Any -> Boolean))
(define (boolean-token? token)
  (token-type? token 'BOOLEAN))

(: number-token? (Any -> Boolean))
(define (number-token? token)
  (token-type? token 'NUMBER))

(: character-token? (Any -> Boolean))
(define (character-token? token)
  (token-type? token 'CHARACTER))

(: string-token? (Any -> Boolean))
(define (string-token? token)
  (token-type? token 'STRING))

(: token-type? (Any Symbol -> Boolean))
(define (token-type? token type)
  (and (token? token)
       (eq? (token-name token) type)))

(: token->list (token -> (List Symbol String Location)))
(define (token->list token)
  (list (token-name token) (token-value token) (token-position token)))

(: token-line (token -> (Option Integer)))
(define (token-line token)
  (and (pair? (token-position token))
       (car (token-position token))))

(: token-column (token -> (Option Integer)))
(define (token-column token)
  (and (pair? (token-position token))
       (cdr (token-position token))))

(: next-location (Input-Port -> Location))
(define (next-location in)
  (let-values (((line-number line-column position) 
                (port-next-location in)))
              (cons line-number line-column)))

(: peek-char/no-eof (Input-Port -> Char))
(define (peek-char/no-eof in)
  (let ([c (peek-char in)])
    (if (eof-object? c)
        (error "unexpected eof")
        c)))

(: read-char/no-eof (Input-Port -> Char))
(define (read-char/no-eof in)
  (let ([c (read-char in)])
    (if (eof-object? c)
        (error "unexpected eof")
        c)))

(define-syntax syntax-error
  (syntax-rules ()
    [(_ location near)
     (error "bad syntax on line"
            (car location)
            'near
            near)]))

(define-syntax push!
  (syntax-rules ()
    [(_ x xs)
     (set! xs (cons x xs))]))

(define-syntax pop!
  (syntax-rules ()
    [(_ xs)
     (let ([x (car xs)])
       (set! xs (cdr xs))
       x)]))
