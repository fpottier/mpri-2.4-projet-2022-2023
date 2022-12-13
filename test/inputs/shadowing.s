(* f(x) = (x+1)^2 + 1 *)
let f (x : real) =
  let y =
    let x = x + 1 in
    let x = x * x in
    x
  in
  y + 1
