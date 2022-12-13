open PPrint
open Surface

(**[print_expr] converts a Surface expression to a PPrint document. *)
val print_expr : expr -> document

(**[print_program] converts a Surface program to a PPrint document. *)
val print_program : prog -> document
