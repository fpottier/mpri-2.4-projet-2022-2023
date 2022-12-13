open Surface

(**[print_type] converts a type to a string. *)
val print_type : ty -> string

(**This exception can be raised by [infer] and [check_expr]. *)
exception Failure of string

(**[on_failure handle f] evaluates [f()] and returns its result.
   If [Failure msg] is raised, then [handle msg] is invoked. *)
val on_failure: (string -> 'a) -> (unit -> 'a) -> 'a

(**[infer prog] verifies that the program [prog] is well-formed and
   well-typed. If so, a function type environment is returned. *)
val infer : prog -> fenv

(**[check_expr fenv e ty] checks that the closed expression [e]
   has type [ty] under the environment [fenv]. *)
val check_expr : fenv -> expr -> ty -> unit
