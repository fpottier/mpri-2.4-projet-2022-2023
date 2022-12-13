open Linear
open LinearHelp
open LinearHelp.Renamings

(* -------------------------------------------------------------------------- *)

(* Bindings. *)

(* Every bound variable is replaced with a fresh variable. *)

let transform_ubinding (_, ty : ubinding) : ubinding =
  ufreshb ty

let transform_ubindings =
  map transform_ubinding

let transform_lbinding (_, ty : lbinding) : lbinding =
  lfreshb ty

let transform_lbindings =
  map transform_lbinding

let enter_bindings rho ubs lbs =
  let ubs' = transform_ubindings ubs
  and lbs' = transform_lbindings lbs in
  let rho' = extend rho ubs (map fst ubs') lbs (map fst lbs') in
  rho', ubs', lbs'

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

(* [transform_expr rho e] returns an expression which is equivalent to
   [rho(e)] (that is, where every free variable has been renamed
   according to [rho]) and where every bound variable has been freshly
   renamed. *)

let rec transform_expr (rho : rho) (e : expr) : expr =
  match e with
  | Loc (e, range) ->
      Loc (transform_expr rho e, range)
  | Ret (uxs, lxs) ->
      Ret (map (uapply rho) uxs, map (lapply rho) lxs)
  | Let (ubs, lbs, e1, e2) ->
      let rho', ubs', lbs' = enter_bindings rho ubs lbs in
      Let (ubs', lbs', transform_expr rho e1, transform_expr rho' e2)
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
  | UTupleElim (ubs, x1, e2) ->
      let rho', ubs', _ = enter_bindings rho ubs [] in
      UTupleElim (ubs', uapply rho x1, transform_expr rho' e2)
  | LTupleIntro lxs ->
      LTupleIntro (map (lapply rho) lxs)
  | LTupleElim (lbs, x1, e2) ->
      let rho', _, lbs' = enter_bindings rho [] lbs in
      LTupleElim (lbs', lapply rho x1, transform_expr rho' e2)
  | FunCall (f, uxs, lxs) ->
      FunCall (f, map (uapply rho) uxs, map (lapply rho) lxs)

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

let transform_decl decl =
  clear();
  match decl with
  | Decl (range, f, ubs, lbs, e) ->
      let rho = identity in
      let rho', ubs', lbs' = enter_bindings rho ubs lbs in
      Decl (range, f, ubs', lbs', transform_expr rho' e)

let transform prog =
  fresh_names_in_namespace "h";
  map transform_decl prog
