open Linear

(**[transform prog] transforms the program [prog] by normalizing the
   body of every function. In a transformed expression, a binding
   construct ([Let], [UTupleElim], [LTupleElim]) can never be nested
   in the left-hand side of a [Let] construct.

   In the program [prog], inside every function, two distinct local
   variables must never have the same name. This property can be
   established by using [Freshen.freshen]. *)
val transform : prog -> prog
