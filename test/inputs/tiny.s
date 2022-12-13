let square (x : real) =
  x * x

let norm2 (v : [real, real]) =
  let [x, y] = v in
  square(x) + square(y)
