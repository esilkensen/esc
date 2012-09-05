module Lambda {
  S ::= Stmt S | Stmt;
  Stmt ::= Assign ";" | Term ";";
  Assign ::= Var "=" Term;
  Term ::= Var | App | Abs | "(" Term ")";
  Var ::= #rx"[a-z]+";
  Abs ::= "lambda" Var "." Term;
  App ::= Term Term [left];
}
