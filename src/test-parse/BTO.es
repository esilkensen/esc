module BTO {
  input ::= VAR "in" param_list "out" param_list "{" prog "}";
  param ::= VAR ":" type;
  param_list ::= param | param "," param_list;
  orientation ::= "row" | "column";
  type ::= orientation "matrix"
         | orientation "vector"
         | "matrix" | "vector" | "scalar";
  prog ::= stmt | prog stmt;
  stmt ::= VAR "=" expr;
  expr ::= NUM 
         | VAR
         | expr "+" expr [left 1]
         | expr "-" expr [left 1]
         | expr "*" expr [left 2]
         | "-" expr [3]
         | expr "'" [4]
         | "(" expr ")";
  VAR ::= #rx"^[a-zA-Z][a-zA-Z0-9]*$";
  NUM ::= #rx"^[0-9]+(.[0-9]*)?$";
}
