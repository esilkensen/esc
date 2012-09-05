import ML, Fun;

let succ = fun x : Int { x + 1 } {
  let n = succ 6 {
    print 2 + n * 5 + 5
  }
}
