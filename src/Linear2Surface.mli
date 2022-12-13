(**[translate] translates a Linear program into a Surface program. The Linear
   program must lie in the unrestricted fragment of Linear. If this is not the
   case, [Forget] must be used first.

   In Linear, a function can return multiple results at once. In Surface,
   a function returns exactly one result. Thus, during the translation,
   a Linear result of arity 1 is translated to a Surface value, and
   a Linear result of arity other than 1 is translated to a tuple. *)
val translate : Linear.prog -> Surface.prog
