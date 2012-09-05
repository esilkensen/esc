#lang racket

(require "private/es.rkt")

(let ([argv (current-command-line-arguments)])
  (if (= 0 (vector-length argv))
      (printf "Usage: ~a <source files>~n"
              (find-system-path 'run-file))
      (for ([filename argv])
        (compile-file filename))))
