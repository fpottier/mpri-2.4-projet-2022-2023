(* Names of toplevel functions. Used in Surface and Linear. *)

type name =
  string

type names =
  name list

(* Sets of names. *)

module NameSet =
  Set.Make(struct
    type t = name
    let compare = String.compare
  end)
