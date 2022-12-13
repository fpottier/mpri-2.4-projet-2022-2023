open Surface
open Env

(**A value is either a real number or a tuple of values. *)
type value =
  | VReal of real
  | VTuple of values

and values =
  value list

(**[print_value] converts a value to a string. *)
val print_value : value -> string

(**[RuntimeError] is raised by [eval] when encountering an ill-formed
   or ill-typed program. *)
exception RuntimeError of range

(**[fail range format ...] prints an error message and raises
   [RuntimeError range]. *)
val fail : range -> ('a, unit, string, 'b) format4 -> 'a

(**[real] transforms a real number into a value. *)
val real: real -> value

(**[as_real range] projects [VReal c] to [c] and fails if its argument
   is not of the form [VReal _]. [range] is used in the error message. *)
val as_real : range -> value -> real

(**[as_tuple range] projects [VTuple vs] to [vs] and fails if its argument is
   not of the form [VTuple _]. [range] is used in the error message. *)
val as_tuple : range -> value -> values

(**[eval_unop] defines the meaning of unary operators. *)
val eval_unop : unop -> real -> real

(**[eval_binop] defines the meaning of binary operators. *)
val eval_binop : binop -> real -> real -> real

(**[lookup range kind env x] looks up the name [x] in the environment [env].
   If the name [x] is unbound, the lookup fails. [range] and [kind] are used
   in the error message. *)
val lookup : range -> string -> 'a env -> name -> 'a

(**[bind_many range kind env xs vs] extends the environment [env] with
   mappings of [xs] to [vs]. If the lists [xs] and [vs] do not have the same
   length, then this fails. [range] and [kind] are used in the error
   message. *)
val bind_many : range -> string -> 'a env -> name list -> 'a list -> 'a env

(**[eval prog env e] evaluates the expression [e] in the context of the
   function definitions found in the program [prog] and in the local variable
   environment [env]. *)
val eval : prog -> value env -> expr -> value
