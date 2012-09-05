module Param {
  S ::= Exp | Exp ";" S;
  Exp ::= Id | Int | Bool;
  Id ::= #rx"^[A-Za-z][A-Za-z0-9]*$";
  Int ::= #rx"^[0-9]+$" | Int "+" Int [left];
  Bool ::= "#true" | "#false";
  forall T.
    T ::= "if" Bool "then" T "else" T;
  forall T1 T2.
    T2 ::= "let" x:Id "=" T1 { x:T1; T2 };
}
