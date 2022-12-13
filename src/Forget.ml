open Linear
open LinearHelp

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* We transform a linear variable into an unrestricted by the same name. *)

let transform_var (L x : lvar) : uvar =
  U x

let transform_binding (x, ty : lbinding) : ubinding =
  (transform_var x, ty)

let transform_vars =
  map transform_var

let transform_bindings =
  map transform_binding

(* -------------------------------------------------------------------------- *)

(* Building an unrestricted zero of an arbitrary type. *)

let rec uzero (ty : ty) : expr =
  match ty with
  | TUnknown ->
      assert false
  | TReal ->
      ULiteral Real.zero
  | TTuple tys ->
      with_uvars (map uzero tys) @@ fun uxs ->
      UTupleIntro uxs

(* -------------------------------------------------------------------------- *)

(* The following functions check that no name collisions are created by
   transforming a linear variable into an unrestricted variable. *)

(* We keep a set of all unrestricted variables in scope, a set of all
   linear variables in scope (mapped to unrestricted variables), and
   we check that these two sets remain disjoint at all times. *)

type env =
  UVarSet.t * UVarSet.t

let empty : env =
  UVarSet.(empty, empty)

let wf (env : env) =
  let (uenv, lenv) = env in
  assert (UVarSet.disjoint uenv lenv);
  env

let ubind (env : env) (ubs : ubindings) : env =
  let (uenv, lenv) = env in
  let uxs = map fst ubs in
  (UVarSet.union uenv (UVarSet.of_list uxs), lenv)
  |> wf

let lbind (env : env) (lbs : lbindings) : env =
  let (uenv, lenv) = env in
  let lxs = map fst lbs in
  let uxs = transform_vars lxs in
  (uenv, UVarSet.union lenv (UVarSet.of_list uxs))
  |> wf

let rec check (env : env) (e : expr) : unit =
  match e with
  | Loc (e, _range) ->
      check env e
  | Let (ubs, lbs, e1, e2) ->
      check env e1;
      let env = ubind env ubs in
      let env = lbind env lbs in
      check env e2
  | Ret _
  | ULiteral _
  | UUnOp _
  | UBinOp _
  | LZero _
  | LAdd _
  | LMul _
  | Drop _
  | Dup _
  | UTupleIntro _
  | LTupleIntro _
  | FunCall _
      -> ()
  | UTupleElim (ubs, _, e2) ->
      let env = ubind env ubs in
      check env e2
  | LTupleElim (lbs, _, e2) ->
      let env = lbind env lbs in
      check env e2

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

let rec transform_expr (e : expr) : expr =
  match e with
  | Loc (e, range) ->
      Loc (transform_expr e, range)
  | Ret (uxs, lxs) ->
      Ret (uxs @ transform_vars lxs, [])
  | Let (ubs, lbs, e1, e2) ->
      Let (ubs @ transform_bindings lbs, [], transform_expr e1, transform_expr e2)
  | ULiteral _
  | UUnOp _
  | UBinOp _
  | UTupleIntro _
        -> e
  | LZero ty ->
      uzero ty
  | LAdd (x1, x2) ->
      UBinOp (transform_var x1, OpAdd, transform_var x2)
  | LMul (x1, x2) ->
      UBinOp (x1, OpMul, transform_var x2)
  | Drop _x ->
      Ret ([], [])
  | Dup x ->
      let x = transform_var x in
      Ret ([x; x], [])
  | UTupleElim (uxs, x1, e2) ->
      UTupleElim (uxs, x1, transform_expr e2)
  | LTupleIntro lxs ->
      UTupleIntro (transform_vars lxs)
  | LTupleElim (lbs, x1, e2) ->
      UTupleElim (transform_bindings lbs, transform_var x1, transform_expr e2)
  | FunCall (f, uxs, lxs) ->
      FunCall (f, uxs @ transform_vars lxs, [])

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

let transform_decl decl =
  clear();
  match decl with
  | Decl (range, f, ubs, lbs, e) ->
      let env = empty in
      let env = ubind env ubs in
      let env = lbind env lbs in
      check env e;
      Decl (range, f, ubs @ transform_bindings lbs, [], transform_expr e)

let transform prog =
  fresh_names_in_namespace "z";
  map transform_decl prog
