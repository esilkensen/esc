import TIL;

var n;
n ::= readint("> ");
var x; 
var fact;
fact ::= 1;
for x ::= 1 to n do
  fact ::= x * fact;
end
write("factorial of ");
writeint(n);
write(" is ");
writeint(fact);
write("\n");
