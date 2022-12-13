(* -------------------------------------------------------------------------- *)

(* Types. *)

(* The types include the base type [real] and tuple types. *)

(* The type [unknown] can be used as a placeholder in a [let] binding, when
   one does not wish to provide a type. It cannot be used elsewhere (e.g.,
   it cannot be nested inside a tuple type, and cannot appear in the
   declaration of a function parameter). *)

type ty = Surface.ty =
  | TUnknown
  | TReal
  | TTuple of tys

and tys =
  ty list

(* A multi-value type. *)

(* A multi-value type describes the output of a box: it is a pair of a list of
   the types of the unrestricted output wires and a list of the types of the
   linear output wires. *)

type mvty =
  tys * tys

let ufrag (tys, _ : mvty) : tys =
  tys

let lfrag (_, tys : mvty) : tys =
  tys

(* A function type. *)

(* A function type is a pair of two multi-value types, which describe the
   function's arguments and results. *)

type fty =
  mvty * mvty

let domain (ty, _ : fty) : mvty =
  ty

let codomain (_, ty : fty) : mvty =
  ty

(* -------------------------------------------------------------------------- *)

(* Real numbers. *)

type real =
  Real.real

(* Toplevel function names. *)

include Name

(* Unrestricted variables. *)

type uvar =
  U of string [@@unboxed]

type uvars =
  uvar list

(* Linear variables. *)

type lvar =
  L of string [@@unboxed]

type lvars =
  lvar list

(* -------------------------------------------------------------------------- *)

(* A function type environment maps function names to function types. *)

type fenv =
  fty Env.env

let flookup (fenv : fenv) (f : name) : fty =
  Env.lookup fenv f (* can raise [Not_found] *)

let fbind (fenv : fenv) (f : name) (fty : fty) : fenv =
  Env.bind fenv f fty

(* -------------------------------------------------------------------------- *)

(* A binding is a pair of a variable and a type. *)

type ubinding =
  uvar * ty

type ubindings =
  ubinding list

type lbinding =
  lvar * ty

type lbindings =
  lbinding list

type bindings =
  ubindings * lbindings

(* -------------------------------------------------------------------------- *)

(* Unary operators. *)

type unop = Surface.unop =
  | OpSin
  | OpCos
  | OpExp

(* Binary operators. *)

type binop = Surface.binop =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv

(* -------------------------------------------------------------------------- *)

(* Source code ranges. *)

type range =
  | Source of Surface.range
  | Virtual of int

let next_virtual_range =
  let (next, _clear) = Gensym.make () in
  fun () ->
    Virtual (next ())

module RangeSet = Set.Make(struct
  type t = range
  let compare = Stdlib.compare
end)

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

(* See Figure 2 in the paper. *)

(* An expression in this language can be thought of as a data flow graph, or a
   "circuit", a "box", with a number of input wires (represented by the free
   variables) and a number of output wires (represented by its multi-value
   result). Each (input or output) wire is classified as either unrestricted
   or linear. Whereas input wires are named (via variables), output wires are
   implicitly numbered (via their position in a multi-value result). *)

(* The syntax of the language is very permissive, but some combinations do not
   make sense: the arity (that is, the number of wires) of each box must be
   respected, both on the input side on and the output side; the unrestricted
   or linear nature of each wire must be respected; the type of each wire must
   be respected; and a linear variable must be used exactly once. These
   constaints are verified by the Linear type-checker. *)

type expr =
  | Loc of expr * range

  (* Multi-value return. *)
  (* This can be viewed as a trivial box, whose (named) input wires
     are directly connected to its (numbered) output wires. *)
  | Ret of uvars * lvars

  (* Multi-value let. *)
  (* This can be viewed as a sequential composition of two boxes, connecting
     the first box's output wires to some of the second box's input wires. *)
  | Let of ubindings * lbindings * expr * expr

  (* A constant. *)
  (* This is a box with no input wires and a single unrestricted output wire. *)
  | ULiteral of real

  (* A unary operator. *)
  (* This is a box with one unrestricted input wire and one unrestricted
     output wire. *)
  | UUnOp of unop * uvar

  (* A binary operator. *)
  (* This is a box with two unrestricted input wires and one unrestricted
     output wire. *)
  | UBinOp of uvar * binop * uvar

  (* Linear zero. *)
  (* This is a box with no input wires and a single linear output wire. *)
  (* Zero is polymorphic: it can have type "real" or "pair of reals", for
     instance. To facilitate bottom-up type-checking, it carries a type. *)
  | LZero of ty

  (* Linear addition. *)
  (* This is a box with two linear input wires and a single linear output wire. *)
  (* Linear addition is polymorphic: it can operate at type "real" or "pair of
     reals", for instance. No type annotation is needed. *)
  | LAdd of lvar * lvar

  (* Linear multiplication. *)
  (* This is a box with one unrestricted input wire, one linear input wire,
     and a single linear output wire. *)
  | LMul of uvar * lvar

  (* Linear drop. *)
  (* This is a box with one linear input wire and no output wire. *)
  | Drop of lvar

  (* Linear dup. *)
  (* This is a box with one linear input wire and two linear output wires. *)
  (* It is often referred to as a "fan-out": one wire fans out into two wires. *)
  | Dup of lvar

  (* Unrestricted tuple construction. *)
  (* This is a box with n unrestricted input wires and one unrestricted
     output wire, whose type is an n-tuple. *)
  | UTupleIntro of uvars

  (* Unrestricted tuple deconstruction / sequential composition. *)
  (* [UTupleElim (bs, x, e)] adapts the box [e], on the input side, by
     splitting the unrestricted input wire [x] (whose type is an n-tuple)
     into n unrestricted input wires [bs]. *)
  | UTupleElim of ubindings * uvar * expr

  (* Linear tuple construction. *)
  (* This is a box with n linear input wires and one linear output wire,
     whose type is an n-tuple. *)
  | LTupleIntro of lvars

  (* Linear tuple deconstruction / sequential composition. *)
  (* [LTupleElim (bs, x, e)] adapts the box [e], on the input side, by
     splitting the linear input wire [x] (whose type is an n-tuple)
     into n linear input wires [bs]. *)
  | LTupleElim of lbindings * lvar * expr

  (* Function call. *)
  (* This is a box whose number of (unrestricted and linear) input and output
     wires must correspond to the arity of the function that is called. *)
  | FunCall of name * uvars * lvars

type exprs =
  expr list

(* -------------------------------------------------------------------------- *)

(* Toplevel function declarations. *)

(* Functions are not recursive. A function can call only a function
   that has been defined earlier. *)

type decl =
  | Decl of range * name * ubindings * lbindings * expr

type decls =
  decl list

(* A program is an ordered list of toplevel function declarations. *)

type prog =
  decls

(* -------------------------------------------------------------------------- *)

(* Destructors. *)

let name (decl : decl) : name =
  match decl with
  | Decl (_, f, _, _, _) ->
      f

let names (decls : decls) : names =
  List.map name decls
