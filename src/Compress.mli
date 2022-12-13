open Surface

(**[transform prog] transforms the program [prog] into an equivalent
   program that uses fewer intermediate variables. If a variable is
   used at most once, then it is replaced with its definition. *)
val transform: prog -> prog
