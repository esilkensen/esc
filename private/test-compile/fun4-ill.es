import ML, Fun;

let fact = 
  fix fun f : Int -> Bool {
        fun n : Int {
          if n < 2 then 1
          else n * f (n - 1)
        }
      }
{
  print fact 5
}
