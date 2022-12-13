open Linear

(**[transform] expects a program [p] where the linearity discipline is not
   necessarily respected, that is, a program where a linear variable can be
   used zero, one or more times. It returns an equivalent program where the
   discipline is respected. This is achieved by inserting [dup] and [drop]
   instructions where necessary. *)
val transform: prog -> prog
