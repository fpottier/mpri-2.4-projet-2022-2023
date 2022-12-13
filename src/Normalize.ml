open Linear
open LinearHelp

let udisjoint (ubs : ubindings) (uxs : UVarSet.t) =
  UVarSet.disjoint (UVarSet.of_list (map fst ubs)) uxs

let ldisjoint (lbs : lbindings) (lxs : LVarSet.t) =
  LVarSet.disjoint (LVarSet.of_list (map fst lbs)) lxs

(* -------------------------------------------------------------------------- *)

(* Contexts. *)

(* A context is an expression with a hole. It is represented as a function
   [fill], of type [expr -> expr], which describes how to fill this hole with
   an expression. *)

(* We also keep track of the free variables of a context, in the fields [uvs]
   and [lvs]. These fields are used only in the assertions that guarantee the
   absence of name collisions; they are otherwise not needed. *)

type ctx = {
  (* The variables that occur free in this context. *)
  uvs : UVarSet.t;
  lvs : LVarSet.t;
  (* The context itself. *)
  fill: expr -> expr
}

let empty = {
  uvs = UVarSet.empty;
  lvs = LVarSet.empty;
  fill = fun e -> e
}

(* -------------------------------------------------------------------------- *)

(* [transform_expr e k] returns an expression which is equivalent to [k[e]]
   --- that is, the expression [e] placed in the context [k] --- and is in
   A-normal form.

   An A-normal expression does not have a [let] construct nested in the
   left-hand side of a [let] construct.

   The expression [k[e]] can also be thought of as [let e in k], that is,
   roughly, first do [e], then, with the result of [e], do [k]. This helps
   understand what is going on here.

   This CPS formulation of A-normal form conversion is standard and goes back
   to Flanagan et al.'s paper (1993). See e.g.
   https://matt.might.net/articles/a-normalization/

   In the following code, the continuation [k] is never applied to a binding
   form: [Let], [UTupleElim], [LTupleElim]. It is always applied to an atomic
   expression.

   In each of the three nontrivial cases, [Let], [UTupleElim], [LTupleElim],
   the continuation [k] is pushed down into the scope of the binding form.
   (A dual way to look at it is that the binding form is hoisted out of the
   context [k].) There is a danger of name capture if the names introduced
   by this binding form occur free in [k].

   At the moment, we check that there is no name collision. If there is a
   collision, however, we do not avoid it; we fail. Using [Freshen.transform]
   before [transform_expr] should guarantee the absence of collisions. *)

let rec transform_expr (e : expr) (k : ctx) : expr =
  match e with
  | Loc (e, _range) -> transform_expr e k
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
      (* An atomic expression. *)
      -> k.fill e
  | Let (ubs, lbs, e1, e2) ->
      assert (udisjoint ubs k.uvs);
      assert (ldisjoint lbs k.lvs);
      (* This can be read from left to right as follows: first do [e1];
         then, with the result of [e1], bind [ubs] and [lbs]; then do [e2];
         then, with the result of [e2], do [k]. *)
      transform_expr e1 (transform_let ubs lbs e2 k)
  | UTupleElim (ubs, x1, e2) ->
      assert (udisjoint ubs k.uvs);
      UTupleElim (ubs, x1, transform_expr e2 k)
  | LTupleElim (lbs, x1, e2) ->
      assert (ldisjoint lbs k.lvs);
      LTupleElim (lbs, x1, transform_expr e2 k)

(* [transform_let ubs lbs e2 k] constructs (a transformd form) of
   the context [let ubs; lbs = [] in let e2 in k]. *)

and transform_let ubs lbs e2 (k : ctx) : ctx =
  let fill e1 =
    Let (ubs, lbs, e1, transform_expr e2 k)
  and uvs =
    UVarSet.(remove_ubindings ubs (fuv e2) + k.uvs)
  and lvs =
    LVarSet.(remove_lbindings lbs (flv e2) + k.lvs)
  in
  { uvs; lvs; fill }

(* -------------------------------------------------------------------------- *)

(* Normalizing declarations. *)

let transform_decl (decl : decl) : decl =
  clear();
  match decl with
  | Decl (range, f, ubs, lbs, e) ->
      Decl (range, f, ubs, lbs, transform_expr e empty)

let transform_decls decls : decls =
  map transform_decl decls

let transform prog : prog =
  no_fresh_names();
  transform_decls prog
