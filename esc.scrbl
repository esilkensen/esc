#lang scribble/manual
@(require planet/scribble scribble/bnf)

@title{@exec{esc}: Extensible Syntax Compiler}
@author{@(author+email "Erik Silkensen" "ejs@ccs.neu.edu")}
@defmodule/this-package[main]

This module

@section{Examples}

As a quick introduction, this section shows an example

 @BNF[(list @nonterm{prog}
            @BNF-seq[@litchar{import} @nonterm{id-seq} @litchar{;}
                     @nonterm{body}])
      (list @nonterm{id-seq}
            @BNF-alt[@nonterm{id}
                     @BNF-seq[@nonterm{id-seq} @litchar{,} @nonterm{id}]])
      (list @nonterm{body}
            @BNF-alt[@nonterm{decl} @nonterm{scope} @elem{any other text}])]

@filebox["ML.es"]{
@verbatim|{
module ML {
  types {
    Type ::= "Int" == Integer;
    Type ::= "Bool" == Boolean;
  }

  // functions
  Int ::= "|" x:Int "|" = (abs x);
  Int ::= x:Int "+" y:Int [left 1] = (+ x y);
  Int ::= x:Int "-" y:Int [left 1] = (- x y);
  Int ::= x:Int "*" y:Int [left 2] = (* x y);
  Bool ::= x:Int "<" y:Int = (< x y);
  forall T.
    Void ::= "print" x:T = (displayln x);

  // macros
  forall T.
    T ::= "if" test:Bool "then" e1:T "else" e2:T =>
      (if test e1 e2);
  forall T1 T2.
    T2 ::= "let" x:Id "=" e1:T1 { x:T1; e2:T2 } =>
      (let: ([x : T1 e1]) e2);
  forall T1 T2.
    T2 ::= e1:T1 ";" e2:T2 [left] => (begin e1 e2);
  forall T. 
    T ::= "(" x:T ")" => x;
  
  // tokens
  Int ::= #rx"^[0-9]+$";
  Id ::= #rx"^[a-zA-Z_][a-zA-Z0-9_]*$";
}
}|
}

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
