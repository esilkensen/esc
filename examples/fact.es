import ML, Functions;

let fact = 
  fix fun f : Int -> Int {
        fun n : Int {
          if n < 2 then 1
          else n * f (n - 1)
        }
      }
{
  print (fact 3)
}
