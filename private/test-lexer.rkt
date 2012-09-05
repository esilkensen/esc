;;;;;; test-lexer.rkt - Lexical Analyzer test module.    -*- Mode: Racket -*-
;;;;;; Author: Erik Silkensen <eriksilkensen@gmail.com>
;;;;;; Version: 16 Jan 2012

#lang racket

(require "lexer.rkt")

(printf ";; Lexical Analyzer test REPL~n")
(printf ";; Use Ctrl-D (i.e., EOF) to exit.~n")
(port-count-lines! (current-input-port))
(let ([prompt "> "])
  (display prompt)
  (flush-output)
  (let repl ([tok (read-token (current-input-port) "#<test-lexer")])
    (if tok
        (begin
          (printf "\r~a [line=~a col=~a src=~a]\n~a"
                  (token-name tok)
                  (token-line tok) (token-column tok)
                  (token-source tok) prompt)
          (flush-output)
          (repl (read-token (current-input-port) "#<test-lexer>")))
        (newline))))
