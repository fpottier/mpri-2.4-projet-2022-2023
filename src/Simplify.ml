open Linear
open LinearHelp
open LinearHelp.Renamings

(* -------------------------------------------------------------------------- *)

(* [is_ret e] determines whether the expression [e] is a [Ret] expression.
   Any [Loc] constructors that are in the way are ignored and discarded. *)

let rec is_ret (e : expr) : (uvars * lvars) option =
  match e with
  | Loc (e, _) ->
      is_ret e
  | Ret (uxs, lxs) ->
      Some (uxs, lxs)
  | _ ->
      None

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

(* [transform_expr rho e] applies the renaming [rho] to the expression [e] and,
   at the same time, performs certain simplifications. *)

let rec transform_expr (rho : rho) (e : expr) : expr =
  match e with
  | Loc (e, range) ->
      Loc (transform_expr rho e, range)

  | Let (ubs, lbs, e1, e2) ->
      begin match is_ret e1 with

      (* When we find a [let] binding of the form [let xs = ys in ...],
         we compose the current renaming [rho] with the renaming [ys/xs]. *)

      | Some (uys, lys) ->
          transform_expr (compose rho ubs uys lbs lys) e2

      (* The remaining cases are routine. *)

      | None ->
          Let (
            ubs, lbs,
            transform_expr rho e1,
            transform_expr (restrict rho ubs lbs) e2
          )
      end
  | Ret (uxs, lxs) ->
      Ret (map (uapply rho) uxs, map (lapply rho) lxs)
  | ULiteral _ ->
      e
  | UUnOp (op, x) ->
      UUnOp (op, uapply rho x)
  | UBinOp (x1, op, x2) ->
      UBinOp (uapply rho x1, op, uapply rho x2)
  | LZero _ ->
      e
  | LAdd (x1, x2) ->
      LAdd (lapply rho x1, lapply rho x2)
  | LMul (x1, x2) ->
      LMul (uapply rho x1, lapply rho x2)
  | Drop x ->
      Drop (lapply rho x)
  | Dup x ->
      Dup (lapply rho x)
  | UTupleIntro uxs ->
      UTupleIntro (map (uapply rho) uxs)
  | UTupleElim (ubs, x, e2) ->
      UTupleElim (ubs, uapply rho x, transform_expr (restrict rho ubs []) e2)
  | LTupleIntro lxs ->
      LTupleIntro (map (lapply rho) lxs)
  | LTupleElim (lbs, x, e2) ->
      LTupleElim (lbs, lapply rho x, transform_expr (restrict rho [] lbs) e2)
  | FunCall (f, uxs, lxs) ->
      FunCall (f, map (uapply rho) uxs, map (lapply rho) lxs)

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

let transform_decl (decl : decl) : decl =
  clear();
  match decl with
  | Decl (range, f, ubs, lbs, e) ->
      Decl (range, f, ubs, lbs, transform_expr identity e)

let transform_decls decls : decls =
  map transform_decl decls

let transform prog : prog =
  no_fresh_names();
  transform_decls prog
