open Linear
open LinearHelp

let eval_unop, eval_binop =
  SurfaceInterpreter.(eval_unop, eval_binop)

type value = SurfaceInterpreter.value =
  | VReal of real
  | VTuple of values

and values =
  value list

let real c =
  VReal c

exception RuntimeError = SurfaceInterpreter.RuntimeError

let as_real, as_tuple, print_value =
  SurfaceInterpreter.(as_real, as_tuple, print_value)

type uenv =
  value Env.env

type lenv =
  value Env.env

type env =
  uenv * lenv

type fenv =
  decl Env.env

let empty, bind =
  Env.(empty, bind)

let lookup, bind_many =
  SurfaceInterpreter.(lookup, bind_many)

(* [range] is [dummy] everywhere in this interpreter. We do not attempt
   to explain where runtime errors occur. They should never occur anyway,
   since we type-check programs prior to running them. *)

let range =
  Surface.dummy

let ulookup uenv (U x) =
  lookup range "unrestricted variable" uenv x

let ubind_many kind uenv bs vs =
  bind_many range kind uenv
    (map (fun (U x, _ty) -> x) bs)
    vs

let llookup lenv (L x) =
  lookup range "linear variable" lenv x

let lbind_many kind lenv xs vs =
  bind_many range kind lenv
    (map (fun (L x, _ty) -> x) xs)
    vs

let bind_many (uenv, lenv) (ubs, lbs) (uvs, lvs) =
  ubind_many "an unrestricted part" uenv ubs uvs,
  lbind_many "a linear part" lenv lbs lvs

let flookup fenv f =
  lookup range "function" fenv f

let fbind fenv f decl =
  bind fenv f decl

let name_of decl =
  let Decl (_, f, _, _, _) = decl in
  f

(* -------------------------------------------------------------------------- *)

let as_real =
  as_real range

let as_tuple =
  as_tuple range

type result =
  values * values

(* -------------------------------------------------------------------------- *)

(* Polymorphic zero. *)

let rec zero ty =
  match ty with
  | TUnknown ->
      assert false
  | TReal ->
      VReal Real.zero
  | TTuple tys ->
      VTuple (map zero tys)

(* Polymorphic addition. *)

let rec add v1 v2 =
  match v1, v2 with
  | VReal v1, VReal v2 ->
      real Real.(v1 +. v2)
  | VTuple vs1, VTuple vs2 when length vs1 = length vs2 ->
      VTuple (List.map2 add vs1 vs2)
  | _ ->
      SurfaceInterpreter.fail range
        "Invalid arguments in a linear addition: %s +. %s"
        (print_value v1) (print_value v2)

(* -------------------------------------------------------------------------- *)

(* Evaluating expressions. *)

let rec eval_expr (fenv : fenv) (env : env) (e : expr) : result =
  match e with
  | Loc (e, _range) ->
      eval_expr fenv env e

  | Ret (uxs, lxs) ->
      let uenv, lenv = env in
      map (ulookup uenv) uxs, map (llookup lenv) lxs

  | Let (ubs, lbs, e1, e2) ->
      let env = bind_many env (ubs, lbs) (eval_expr fenv env e1) in
      eval_expr fenv env e2

  | ULiteral c ->
      u1 (real c)

  | UUnOp (uop, x) ->
      let uenv, _ = env in
      let v = ulookup uenv x in
      u1 (real (eval_unop uop (as_real v)))

  | UBinOp (x1, bop, x2) ->
      let uenv, _ = env in
      let v1 = ulookup uenv x1
      and v2 = ulookup uenv x2 in
      u1 (real (eval_binop bop (as_real v1) (as_real v2)))

  | LZero ty ->
      l1 (zero ty)

  | LAdd (x1, x2) ->
      let _, lenv = env in
      let v1 = llookup lenv x1
      and v2 = llookup lenv x2 in
      l1 (add v1 v2)

  | LMul (x1, x2) ->
      let uenv, lenv = env in
      let v1 = ulookup uenv x1
      and v2 = llookup lenv x2 in
      l1 (real (Real.( *. ) (as_real v1) (as_real v2)))

  | Drop x ->
      let _, lenv = env in
      let v = llookup lenv x in
      ignore v;
      [], []

  | Dup x ->
      let _, lenv = env in
      let v = llookup lenv x in
      l2 v v

  | UTupleIntro uxs ->
      let uenv, _ = env in
      u1 (VTuple (map (ulookup uenv) uxs))

  | UTupleElim (ubs, x1, e2) ->
      let uenv, lenv = env in
      let uvs = as_tuple (ulookup uenv x1) in
      let uenv = ubind_many "a tuple" uenv ubs uvs in
      let env = uenv, lenv in
      eval_expr fenv env e2

  | LTupleIntro lxs ->
      let _, lenv = env in
      l1 (VTuple (map (llookup lenv) lxs))

  | LTupleElim (lbs, x1, e2) ->
      let uenv, lenv = env in
      let lvs = as_tuple (llookup lenv x1) in
      let lenv = lbind_many "a tuple" lenv lbs lvs in
      let env = uenv, lenv in
      eval_expr fenv env e2

  | FunCall (f, uxs, lxs) ->
      let uenv, lenv = env in
      let vs = map (ulookup uenv) uxs, map (llookup lenv) lxs in
      let Decl (_range, _f, ubs, lbs, body) = flookup fenv f in
      let env = bind_many (empty, empty) (ubs, lbs) vs in
      eval_expr fenv env body

(* -------------------------------------------------------------------------- *)

(* Evaluating programs. *)

let record_decl (fenv : fenv) (decl : decl) : fenv =
  fbind fenv (name_of decl) decl

let record_decls (decls : decls) : fenv =
  List.fold_left record_decl empty decls

let eval (prog : prog) (env : env) (e : expr) : result =
  let fenv = record_decls prog in
  eval_expr fenv env e
