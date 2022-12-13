(* Real numbers. *)

type real =
  Real.real

(* Toplevel functions have names. *)

include Name

(* Local variables have names. *)

type var =
  string

type vars =
  var list

(* The types include the base type [real] and tuple types. *)

(* The type [unknown] can be used as a placeholder in a [let] binding, when
   one does not wish to provide a type. It cannot be used elsewhere (e.g.,
   it cannot be nested inside a tuple type, and cannot appear in the
   declaration of a function parameter). *)

type ty =
  | TUnknown
  | TReal
  | TTuple of ty list

type tys =
  ty list

(* A type environment. *)

type env =
  ty Env.env

(* A function type. *)

type fty =
  tys * ty

(* A function type environment. *)

type fenv =
  fty Env.env

(* A binding is a variable accompanied with an explicit type. *)

type binding =
  var * ty

type bindings =
  binding list

(* Unary operators. *)

type unop =
  | OpSin
  | OpCos
  | OpExp

(* Binary operators. *)

type binop =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv

(* Source code ranges. *)

type range =
  Lexing.position * Lexing.position

let dummy : range =
  (Lexing.dummy_pos, Lexing.dummy_pos)

(* Expressions. *)

type expr =
  | Loc of expr * range
  | Var of var
  | Literal of real
  | UnOp of unop * expr
  | BinOp of expr * binop * expr
  | Let of var * expr * expr
  | TupleIntro of exprs
  | TupleElim of vars * expr * expr
  | FunCall of name * exprs

and exprs =
  expr list

(* Toplevel function declarations. *)

(* Functions are not recursive. A function can call only a function
   that has been defined earlier. *)

type decl =
  | Decl of name * bindings * expr * range

type decls =
  decl list

(* Programs. *)

type prog =
  decls

(* -------------------------------------------------------------------------- *)

(* Constructors. *)


let var x =
  Var x

let vars xs =
  List.map var xs

let lit c =
  Literal c

let lits cs =
  List.map lit cs

(* -------------------------------------------------------------------------- *)

(* Destructors. *)

let name (decl : decl) : name =
  match decl with
  | Decl (name, _, _, _) ->
      name

let names (decls : decls) : names =
  List.map name decls
