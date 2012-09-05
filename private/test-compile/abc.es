import ML, Sets;

let A = {1, 2, 3} { 
  let B = {2, 3, 4} {
    let C = {3, 4, 5} {
      print |A & C|;
      print A | B & C
    }
  }
}
