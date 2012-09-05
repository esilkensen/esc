module PairFun {
  types {
    Type ::= T1:Type "×" T2:Type == (Pairof T1 T2);
    Type ::= T1:Type "->" T2:Type [right] == (T1 -> T2);
  }

  forall T1 T2.
    T1 × T2 ::= "{" fst:T1 "," snd:T2 "}"
      => (cons fst snd);

  forall T1 T2.
    T1 ::= p : T1 × T2 "." "fst"
      => (car p);
 
  forall T1 T2.
    T2 ::= p : T1 × T2 "." "snd"
      => (cdr p);

  forall T2.
    T1 -> T2 ::= "fun" x:Id ":" T1:Type { x:T1; e1:T2 }
      => (λ: ([x : T1]) e1);

  forall T1 T2.
    T2 ::= f : T1 -> T2 x : T1 [left]
      => (f x);

  forall T1 T2.
    T1 -> T2 ::= "fix" f : (T1 -> T2) -> (T1 -> T2)
      = ((λ: ([x : (Rec A (A -> (T1 -> T2)))])
           (f (λ (y) ((x x) y))))
         (λ: ([x : (Rec A (A -> (T1 -> T2)))])
           (f (λ (y) ((x x) y)))));

  Id ::= #px"^[:alpha:][:word:]*$";
}


