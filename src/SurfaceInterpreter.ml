open Printf
let length, unzip = List.(length, split)
open Surface
open Real

(* -------------------------------------------------------------------------- *)

(* Primitive operations. *)

let eval_unop uop =
  match uop with
  | OpSin ->
      sin
  | OpCos ->
      cos
  | OpExp ->
      exp

let eval_binop bop =
  match bop with
  | OpAdd ->
      (+.)
  | OpSub ->
      (-.)
  | OpMul ->
      ( *.)
  | OpDiv ->
      (/.)

(* -------------------------------------------------------------------------- *)

(* Values. *)

type value =
  | VReal of real
  | VTuple of values

and values =
  value list

let real c =
  VReal c

exception RuntimeError of range

let fail range format =
  ksprintf (fun s ->
    fprintf stderr "%s.\n%!" s; raise (RuntimeError range)
  ) format

let as_real range v =
  match v with
  | VReal c ->
      c
  | VTuple vs ->
      fail range "Expected a real; got a tuple of arity %d"
        (length vs)

let as_tuple range v =
  match v with
  | VTuple vs ->
      vs
  | _ ->
      fail range "Expected a tuple; got a real"

let rec print_value v =
  match v with
  | VReal c ->
      to_string c
  | VTuple vs ->
      sprintf "[%s]" (String.concat ", " (List.map print_value vs))

(* -------------------------------------------------------------------------- *)

(* Runtime environments. *)

type env =
  value Env.env

type fenv =
  (bindings * expr) Env.env

let empty, bind =
  Env.(empty, bind)

let lookup range kind env x =
  try
    Env.lookup env x
  with Not_found ->
    fail range "Unbound %s: %s" kind x

let bind_many range kind env xs vs =
  try
    Env.bind_many env xs vs
  with Invalid_argument _ ->
    fail range "Expected %s of arity %d; got %s of arity %d"
      kind (length xs) kind (length vs)

(* -------------------------------------------------------------------------- *)

(* Evaluating expressions. *)

let rec eval range (fenv : fenv) (env : env) (e : expr) : value =
  match e with
  | Loc (e, range) ->
      eval range fenv env e
  | Var x ->
      lookup range "variable" env x
  | Literal c ->
      real c
  | UnOp (uop, e) ->
      real (eval_unop uop (as_real range (eval range fenv env e)))
  | BinOp (e1, bop, e2) ->
      real (eval_binop bop
        (as_real range (eval range fenv env e1))
        (as_real range (eval range fenv env e2))
      )
  | Let (x, e1, e2) ->
      let env = bind env x (eval range fenv env e1) in
      eval range fenv env e2
  | TupleIntro es ->
      VTuple (List.map (eval range fenv env) es)
  | TupleElim (xs, e1, e2) ->
      let vs = as_tuple range (eval range fenv env e1) in
      let env = bind_many range "a tuple" env xs vs in
      eval range fenv env e2
  | FunCall (f, es) ->
      let vs = List.map (eval range fenv env) es in
      let bs, e = lookup range "function" fenv f in
      let xs, _ = unzip bs in
      let env = bind_many range "an argument list" empty xs vs in
      (* Because all functions have distinct names, there is no need to
         restrict [fenv] to the function environment that existed when
         the function [f] was defined.  *)
      eval range fenv env e

(* -------------------------------------------------------------------------- *)

(* Evaluating programs. *)

let eval_decl (fenv : fenv) (decl : decl) : fenv =
  let Decl (f, bs, e, _range) = decl in
  bind fenv f (bs, e)

let eval_decls (decls : decls) : fenv =
  List.fold_left eval_decl empty decls

let eval (prog : prog) (env : env) (e : expr) : value =
  let range = dummy (* not great *)
  and fenv = eval_decls prog in
  eval range fenv env e
