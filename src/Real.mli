(* This module provides real numbers for use when evaluating Surface
   or Linear programs. *)

(* Its implementation is based on computable real numbers, which offer
   guaranteed precision. The functions [near], [close], [/.], and [to_string]
   work at a (fixed, unspecified) limited precision. *)

(* The type of real numbers. *)

type real

(* Constants. *)

val minus_one : real
val zero : real
val one : real
val two : real

(* Conversions of integers and floating-point numbers to real numbers. *)

val of_int : int -> real
val of_float : float -> real

(* Approximate equality tests. *)

(**[near x y] determines whether the numbers [x] and [y] are close to each
   other in an additive sense. *)
val near : real -> real -> bool

(**[close observed expected] attempts to determine whether the numbers
   [observed] and [expected] are close to each other in a multiplicative
   sense. It is based on a comparison between the ratio [observed / expected]
   and the number 1. It is conservative: if it returns [false] then the
   numbers are definitely not close; if it returns [true] then the numbers may
   or may not be close. *)
val close : real -> real -> bool

(* The four traditional arithmetic operations on real numbers. *)

(* Division is conservative: if [y] is near zero then [x /. y] raises the
   exception [NearZero]. *)

val (+.) : real -> real -> real
val (-.) : real -> real -> real
val ( *.) : real -> real -> real
exception NearZero
val (/.) : real -> real -> real

(* Sine, cosine, exponential. *)

val sin : real -> real
val cos : real -> real
val exp : real -> real

(* Printing. *)

val to_string : real -> string
