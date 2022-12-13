(**[transform] translates a well-formed and well-typed Surface program into a
   Linear program. Only the unrestricted fragment of Linear is exploited by
   this translation. *)
val transform : Surface.prog -> Linear.prog
