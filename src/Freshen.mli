open Linear

(**[transform prog] transforms the program [prog] into an equivalent program
   with the property that, inside each function, two distinct local variables
   cannot have the same name.

   This property is currently achieved in a brutal way by renaming every
   local variable to a fresh name. It could conceivably be achieved in a
   more subtle way, by renaming only when necessary. *)
val transform: prog -> prog
