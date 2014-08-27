#lang setup/infotab
(define name "esc: Extensible Syntax Compiler")
(define blurb '("A tool for designing composable DSLs."))
(define primary-file "main.rkt")
(define categories '(devtools metaprogramming misc))
(define repositories '("4.x"))
(define scribblings '(("esc.scrbl" (user-doc) (parsing-library))))
(define raco-commands
  '(("esc" (planet esilkensen/esc) "compile composable DSLs to Racket" #f)))
(define release-notes
  '("Minor update to comply with new Typed Racket syntax."))
