module TIL {
  Program ::= Stats;
  Stats ::= Stat Stats | Stat;
  Stat ::= Declaration | DeclarationTyped | Assign
         | Block | IfThen | IfElse | While | For | ProcCall;
  Declaration ::= "var" Id ";";
  DeclarationTyped ::= "var" Id ":" TILType ";";
  TILType ::= Id;
  Assign ::= Id "::=" Exp ";";
  Block ::= "begin" Stats "end";
  IfThen ::= "if" Exp "then" Stats "end";
  IfElse ::= "if" Exp "then" Stats "else" Stats "end";
  While ::= "while" Exp "do" Stats "end";
  For ::= "for" Id "::=" Exp "to" Exp "do" Stats "end";
  ProcCall ::= Id "(" Exp ")" ";";
  Exp ::= Mul | Div | Mod | Add | Sub 
        | Lt | Gt | Leq | Geq | Equ | Neq
        | And | Or | True | False
        | Id | Int | String | FunCall
        | "(" Exp ")";
  FunCall ::= Id "(" Exp ")";
  Mul ::= Exp "*" Exp;
  Div ::= Exp "/" Exp;
  Mod ::= Exp "%" Exp;
  Add ::= Exp "+" Exp;
  Sub ::= Exp "-" Exp;
  Lt ::= Exp "<" Exp;
  Gt ::= Exp ">" Exp;
  Leq ::= Exp "<=" Exp;
  Geq ::= Exp ">=" Exp;
  Equ ::= Exp "=" Exp;
  Neq ::= Exp "!=" Exp;
  And ::= Exp "&" Exp;
  Or ::= Exp "|" Exp;
  True ::= "true";
  False ::= "false";
  Id ::= #rx"^[A-Za-z][A-Za-z0-9]*$";
  Int ::= #rx"^[0-9]+$";
  String ::= #rx"^[\"][^\"]*[\"]$";
}
