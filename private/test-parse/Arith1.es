module Arith1 {
  S ::= Exp | Exp S;
  Exp ::= Add | Mul | Neg | Name;
  Add ::= Exp "+" Exp [left 1];
  Mul ::= Exp "*" Exp [left 2];
  Neg ::= "-" Exp [3];
  Name ::= #rx"^[A-Z]$";
}
