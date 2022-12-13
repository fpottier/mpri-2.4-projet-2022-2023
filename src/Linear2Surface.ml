open Surface

(* -------------------------------------------------------------------------- *)

(* Variable and function names. *)

let translate_var (Linear.U x) =
  x

let translate_vars uxs =
  List.map translate_var uxs

let translate_binding (ux, _ty) =
  translate_var ux

let translate_bindings ubs =
  List.map translate_binding ubs

let transport_binding (ux, ty) =
  translate_var ux, ty

let transport_bindings ubs =
  List.map transport_binding ubs

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

(* A multi-result of arity 1 is translated to an ordinary result.
   A multi-result of arity other than 1 is translated to a tuple. *)

let rec translate (e : Linear.expr) : expr =
  match e with
  | Linear.Ret ([ux], lxs) ->
      assert (lxs = []);
      Var (translate_var ux)
  | Linear.Ret (uxs, lxs) ->
      assert (lxs = []);
      TupleIntro (vars (translate_vars uxs))
  | Linear.Let ([ub], lbs, e1, e2) ->
      assert (lbs = []);
      Let (translate_binding ub, translate e1, translate e2)
  | Linear.Let (ubs, lbs, e1, e2) ->
      assert (lbs = []);
      TupleElim (translate_bindings ubs, translate e1, translate e2)
  | Linear.ULiteral c ->
      Literal c
  | Linear.UUnOp (op, x) ->
      UnOp (op, var (translate_var x))
  | Linear.UBinOp (x1, op, x2) ->
      BinOp (var (translate_var x1), op, var (translate_var x2))
  | Linear.LZero _
  | Linear.LAdd _
  | Linear.LMul _
  | Linear.Drop _
  | Linear.Dup _
  | Linear.LTupleIntro _
  | Linear.LTupleElim _
      -> assert false
  | Linear.UTupleIntro uxs ->
      TupleIntro (vars (translate_vars uxs))
  | Linear.UTupleElim (ubs, ux1, e2) ->
      TupleElim (translate_bindings ubs, var (translate_var ux1), translate e2)
  | Linear.FunCall (f, uxs, lxs) ->
      assert (lxs = []);
      FunCall (f, vars (translate_vars uxs))
  | Linear.Loc (e, loc) ->
      (match loc with
       | Linear.Virtual _ ->
         translate e
       | Linear.Source range ->
         Loc (translate e, range)
      )

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

let translate_decl decl =
  match decl with
  | Linear.Decl (_range, f, ubs, lbs, e) ->
      assert (lbs = []);
      Decl (f, transport_bindings ubs, translate e, dummy)

let translate decls =
  List.map translate_decl decls
