module Sets {
  types {
    Type ::= T:Type "Set" == (Setof T);
    Type ::= T:Type "Seq" == (Listof T);
  }

  // macros
  forall T.
    T Set ::= "{" x:T "}" => (set x);
  forall T.
    T Set ::= "{" x:T xs:(T Seq) "}" => (list->set (cons x xs));

  forall T.
    T Seq ::= "," x:T => (list x);
  forall T.
    T Seq ::= "," x:T xs:(T Seq) => (cons x xs);

  // functions
  forall T.
    T Set ::= s1:(T Set) "|" s2:(T Set) [left 1] =
      (set-union s1 s2);
  forall T.
    T Set ::= s1 : (T Set) "&" s2:(T Set) [left 2] =
      (set-intersect s1 s2);
  forall T.
    Integer ::= "|" s:(T Set) "|" =
      (set-count s);
}
