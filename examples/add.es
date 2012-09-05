import ML, Functions;

let mk_add = fun x : Int { fun y : Int { x + y } } {
  let succ = mk_add 1 {
    let n = succ 6 {
      print 2 + n * 5 + 5
    }
  }
}
