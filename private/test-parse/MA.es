module MA {
  S ::= Stmt S | Stmt;
  Stmt ::= Exp ";";
  Exp ::= Scalar | Vector | Matrix;
  Scalar ::= "(" Scalar ")"
           | Scalar "*" Scalar [left];
  Vector ::= "(" Vector ")"
           | Vector "*" Scalar [left] | Scalar "*" Vector [left];
  Matrix ::= "(" Matrix ")"
           | Matrix "*" Matrix [left] | Matrix "*" Scalar [left]
           | Scalar "*" Matrix [left] | Matrix "*" Vector [left];
}
