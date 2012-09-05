module Fun {
  types {
    Type ::= T1:Type "->" T2:Type [right] == (T1 -> T2);
  }

  forall T2.
    T1 -> T2 ::= "fun" x:Id ":" T1:Type { x:T1; e1:T2 } =>
      (λ: ([x : T1]) e1);

  forall T1 T2.
    T2 ::= f : T1 -> T2 x:T1 [left] => (f x);
 
  forall T1 T2.
    T1 -> T2 ::= "fix" f : (T1 -> T2) -> (T1 -> T2) =
      ((λ: ([x : (Rec A (A -> (T1 -> T2)))])
         (f (λ (y) ((x x) y))))
       (λ: ([x : (Rec A (A -> (T1 -> T2)))])
         (f (λ (y) ((x x) y)))));

  Id ::= #rx"^[a-zA-Z_][a-zA-Z0-9_]*$";
}
