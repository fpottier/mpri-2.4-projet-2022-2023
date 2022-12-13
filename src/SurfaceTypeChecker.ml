let  length, iter, iter2, unzip = List.(length, iter, iter2, split)
open Printf
open Surface

(* -------------------------------------------------------------------------- *)

(* A basic type printer. *)

let rec print_type ty =
  match ty with
  | TUnknown ->
      "unknown"
  | TReal ->
      "real"
  | TTuple tys ->
      sprintf "[%s]" (String.concat ", " (List.map print_type tys))

(* -------------------------------------------------------------------------- *)

(* A failure function. *)

exception Failure of string

let raise_failure msg =
  raise (Failure msg)

let fail_raw format =
  ksprintf raise_failure format

let fail range format =
  ksprintf (fun msg ->
    fail_raw "%s%s\n" (MenhirLib.LexerUtil.range range) msg
  ) format

let on_failure handle f =
  try
    f()
  with Failure msg ->
    handle msg

(* -------------------------------------------------------------------------- *)

(* Checking that [TUnknown] does not appear in a type. *)

let rec known range ty =
  match ty with
  | TUnknown ->
      fail range "An unknown type appears in a type annotation."
  | TReal ->
      ()
  | TTuple tys ->
      iter (known range) tys

(* -------------------------------------------------------------------------- *)

(* Environments for variables and functions. *)

(* In Surface, a function returns a value.
   In Linear,  a function returns a multi-result. *)

let empty =
  Env.empty

let defensive_lookup range kind env x =
  try
    Env.lookup env x
  with Not_found ->
    fail range "Unbound %s: %s." kind x

let bind range env x ty =
  known range ty;
  Env.bind env x ty

let check_distinct range kind xs =
  try
    Env.check_distinct xs
  with Env.Collision x ->
    fail range "Multiply-defined %s: %s." kind x

let defensive_bind_many range kind env xs tys =
  check_distinct range kind xs;
  iter (known range) tys;
  Env.bind_many env xs tys

(* -------------------------------------------------------------------------- *)

(* Type-checking expressions. *)

let rec infer range (fenv : fenv) (env : env) (e : expr) : ty =
  match e with
  | Loc (e, range) ->
      infer range fenv env e
  | Var x ->
      defensive_lookup range "variable" env x
  | Literal _ ->
      TReal
  | UnOp (_uop, e) ->
      check_real range fenv env e;
      TReal
  | BinOp (e1, _bop, e2) ->
      check_real range fenv env e1;
      check_real range fenv env e2;
      TReal
  | Let (x, e1, e2) ->
      let env = bind range env x (infer range fenv env e1) in
      infer range fenv env e2
  | TupleIntro es ->
      TTuple (List.map (infer range fenv env) es)
  | TupleElim (xs, e1, e2) ->
      let tys1 = check_tuple range fenv env e1 in
      if length xs <> length tys1 then
        fail range "Expected a tuple of arity %d; got a tuple of arity %d."
          (length xs) (length tys1);
      let env = defensive_bind_many range "variable" env xs tys1 in
      infer range fenv env e2
  | FunCall (f, es) ->
      let tys, ty = defensive_lookup range "function" fenv f in
      if length tys <> length es then
        fail range "Expected %d arguments; got %d arguments."
          (length tys) (length es);
      iter2 (check_expected range fenv env) es tys;
      ty

and check_real range fenv env e =
  match infer range fenv env e with
  | TUnknown ->
      assert false
  | TReal ->
      ()
  | TTuple tys ->
      fail range "Expected a real; got a tuple of arity %d."
        (length tys)

and check_tuple range fenv env e : tys =
  match infer range fenv env e with
  | TUnknown ->
      assert false
  | TReal ->
      fail range "Expected a tuple; got a real."
  | TTuple tys ->
      tys

and check_expected range fenv env e (expected : ty) =
  let actual = infer range fenv env e in
  if expected <> actual then
    fail range "Expected type %s; got type %s."
      (print_type expected) (print_type actual)

(* -------------------------------------------------------------------------- *)

(* Type-checking declarations. *)

let infer_decl fenv decl : fenv =
  let Decl (f, bindings, e, range) = decl in
  let xs, tys = unzip bindings in
  let env = defensive_bind_many range "variable" empty xs tys in
  let ty = infer range fenv env e in
  let fty = (tys, ty) in
  Env.bind fenv f fty

let infer_decls fenv decls : fenv =
  List.fold_left infer_decl fenv decls

let infer prog =
  let range = dummy (* not great *) in
  check_distinct range "function" (names prog);
  infer_decls empty prog

let check_expr fenv e expected =
  let range = dummy
  and env = empty
  and handle msg =
    fail_raw "The expression %s either is ill-typed\n\
              or does not have the expected type.\n\
              %s"
      (PPrintExtra.to_string (SurfacePrinter.print_expr e))
      msg
  in
  on_failure handle (fun () -> check_expected range fenv env e expected)
