import ML, Functions;

let mk_square = fun f : Int -> Int { fun x : Int { (f x) * (f x) } } {
  let f = fun x : Int { x + 2 } {
    let g = mk_square f {
      print g 2
    }
  }
}
