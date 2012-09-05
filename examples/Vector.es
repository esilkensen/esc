module Vector {
  types { 
    Type ::= "Vec" == (Vectorof Integer); 
  }

  Vec ::= x:Vec "+" y:Vec [left] =
    (vector-map + x y);
}
