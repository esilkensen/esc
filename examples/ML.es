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
