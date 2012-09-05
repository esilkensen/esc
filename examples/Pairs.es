module Pairs {
  types {
    Type ::= T1:Type "×" T2:Type == (Pairof T1 T2);
  }

  forall T1 T2.
    T1 × T2 ::= "{" fst:T1 "," snd:T2 "}" => (cons fst snd);

  forall T1 T2.
    T1 ::= p:(T1 × T2) "." "fst" => (car p);
 
  forall T1 T2.
    T2 ::= p:(T1 × T2) "." "snd" => (cdr p);
}
