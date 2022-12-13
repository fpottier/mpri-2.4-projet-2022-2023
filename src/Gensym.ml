let postincrement c =
  let x = !c in
  c := x + 1;
  x

let fresh c () =
  postincrement c

let clear c () =
  c := 0

let make () =
  let c = ref 0 in
  fresh c, clear c
