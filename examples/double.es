import ML, Pairs, Functions;

let maybeDouble = 
  fun p : Bool × Int {
    if p.fst then p.snd
    else p.snd * 2
  }
{
  print maybeDouble {1 < 0, 21}
}
