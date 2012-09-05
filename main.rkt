#lang racket

(require raco/command-name "private/es.rkt")

(define source-files
  (command-line
   #:program (short-program+command-name)
   #:args source-file
   source-file))

(let ([argv (current-command-line-arguments)])
  (if (= 0 (vector-length argv))
      (printf "Usage: ~a [<source-file>] ...~n"
              (short-program+command-name))
      (for ([filename argv])
        (compile-file filename))))
