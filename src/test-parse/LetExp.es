module LetExp {
  S ::= Exp | Exp S;
  Exp ::= Let | Add | Id | Int;
  Let ::= "let" Id "=" Exp "in" Exp [1];
  Add ::= Exp "+" Exp [left 2];
  Id ::= #rx"^[A-Za-z][A-Za-z0-9]*$";
  Int ::= #rx"^[0-9]+$";
}
