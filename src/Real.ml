open CReal

type real =
  t

let zero, one, two, of_int, of_float =
  zero, one, two, of_int, of_float

let minus_one =
  of_int (-1)

let (+.), (-.), ( *.) =
  add, sub, mul

let sin, cos, exp =
  sin, cos, exp

(* We use a low precision in the following, so as speed up computations and
   avoid false alarms. The precision is expressed in negative powers of 4;
   i.e., the actual precision is [4^{-precision}]. *)

let precision = 4

let near x y =
  rel_cmp precision x y = 0

exception NearZero

let (/.) x y =
  if near y zero then raise NearZero else div x y

let decimal_precision =
  2 (* 1e-2 is a bit greater than twice 4^{-precision} *)

let to_string x =
  to_string x decimal_precision

let close observed expected =
  try
    (* If possible, compute the ratio [observed / expected], and test
       whether it is near 1. *)
    near one (observed /. expected)
  with NearZero ->
    (* [expected] is near zero. We require [observed] to be near zero
       as well, for a somewhat less strict notion of nearness. *)
    rel_cmp (precision - 1) observed zero = 0
