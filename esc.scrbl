#lang scribble/manual
@(require planet/scribble scribble/bnf (for-label racket))

@title{@exec{esc}: Extensible Syntax Compiler}
@author{@(author+email "Erik Silkensen" "ejs@ccs.neu.edu")}
@defmodule/this-package[main]

This is a project for an extensible parsing system that aims to provide a
user-friendly method for designing @emph{composable} DSLs.

Languages are defined in modules by CFGs written in standard BNF notation.
DSLs can be built on top of the Racket programming language by writing
@emph{type-oriented} grammar rules as function or macro signatures followed
by Racket code for their implementation.

For example, the rule
@verbatim[#:indent 4]{Integer ::= "|" s:Set "|" = (set-count s);}
defines syntax for set cardinality, which is implemented by Racket's
@racket[set-count] function.

@section{Installation}

To try out the tool, first download and install Racket from
@link["http://racket-lang.org/download"]{http://racket-lang.org}.
Then start the Racket interpreter, and enter
@racket[(require (planet esilkensen/esc))].
This installs an @tt{esc} command to the @tt{raco} program.

At the command line,
@verbatim[#:indent 4]{raco esc [<source-file>] ...}
will compile a sequence of extensible syntax files into Typed Racket code.

@section{Examples}

Included in the
@link["http://planet.racket-lang.org/package-source/esilkensen/esc.plt/1/1/examples"]{examples}
directory is a module for giving ML-like syntax to several Typed Racket forms
(@link["http://planet.racket-lang.org/package-source/esilkensen/esc.plt/1/1/examples/ML.es"]{ML.es}),
and another for basic set operations such as the one above
(@link["http://planet.racket-lang.org/package-source/esilkensen/esc.plt/1/1/examples/Sets.es"]{Sets.es}).
@filebox["abc.es"]{
@verbatim|{
import ML, Sets;
let A = {1, 2, 3} {
  let B = {2, 3, 4} {
    let C = {3, 4, 5} {
      print |A & C|;
      print A | B & C
    }
  }
}
}|
}
The above program
(@link["http://planet.racket-lang.org/package-source/esilkensen/esc.plt/1/1/examples/abc.es"]{abc.es})
can be compiled and run with the following commands:
@verbatim[#:indent 4]|{
raco esc abc.es
racket -I typed/racket abc.rkt
}|
The output of the program is
@verbatim[#:indent 4]|{
1
#<set: 1 2 3 4>
}|
