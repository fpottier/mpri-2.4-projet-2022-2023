open Linear

(**This exception is raised by [check]. *)
exception Error of range option * string

(**[check mode prog] checks that the program [prog] is well-typed.

   A moderate amount of bottom-up type inference is performed, as follows. The
   program [prog] may contain a [Let] binding where a variable is annotated
   with the special type [TUnknown]. This annotation is then updated: the
   variable is annotated with its actual (inferred) type.

   [check mode prog] returns an annotated program, which is identical to
   [prog], except that it does not mention the type [TUnknown] any more.

   If [mode] is [`Lenient], then the type-checker checks that every variable
   is bound and that every variable is used at a correct type. If [mode] is
   [`Strict], then, in addition, the type-checker checks that every linear
   variable is used exactly once.

   If a violation of the type-checking discipline is detected, then the
   exception [Error] is raised. *)
val check: [`Lenient | `Strict] -> prog -> prog

(**[environment prog] assumes that the program [prog] is well-typed
   and returns an environment that maps every function name to the
   type of this function. *)
val environment: prog -> fenv
