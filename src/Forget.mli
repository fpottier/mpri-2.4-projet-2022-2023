open Linear

(**[transform] transforms a program into a program where every variable is
   unrestricted. In other words, the "linear" quality of linear variables is
   forgotten. *)
val transform: prog -> prog
