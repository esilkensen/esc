module Arith2 {
  S ::= Exp | Exp S;
  Exp ::= Add | Mul | Neg | Name;
  Add ::= Exp "+" Exp [left 2];
  Mul ::= Exp "*" Exp [left 3];
  Neg ::= "-" Exp [1];
  Name ::= #rx"^[A-Z]$";
}
