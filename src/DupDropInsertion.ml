open Linear
open LinearHelp
open LVarSet
let map = List.map

(* -------------------------------------------------------------------------- *)

(* A renaming [rho] is a total injective mapping of variables to variables. *)

(* It is represented in memory as a finite map, and is the identity
   everywhere else. *)

type rho =
  lvar LVarMap.t

let apply (rho : rho) (x : lvar) : lvar =
  try
    LVarMap.find x rho
  with Not_found ->
    x

(* -------------------------------------------------------------------------- *)

(* [dup z k] creates a context of the form [let (; z1, z2) = dup z in []],
   where the variables [z1] and [z2] are fresh, and wraps the expression
   produced by the meta-level function call [k (z1, z2)] in this context. *)

(* The easiest way to think about this function, and those that follow, is
   to recognize that the program is written in the CPS monad and to think of
   generating a [let/dup] context as a side effect. In this view, [dup z]
   simply returns a pair [(z1, z2)] and generates a [dup] node as a side
   effect. *)

let dup (z : lvar) (k : lvar * lvar -> expr) : expr =
  let z1, z2 = lfresh(), lfresh() in
  Let ([], [(z1, TUnknown); (z2, TUnknown)], Dup z, k (z1, z2))

(* For each variable [x] in the list [xs],
   [dup_many rho xs] splits the variable [z]
   (that is, [rho(xs)])
   into two fresh variables [z1] and [z2].
   It returns the pair of renamings [(rho1, rho2)]
   obtained by overriding [rho]
   with mappings of each [x] to the corresponding [z1] and [z2],
   respectively. *)

let rec dup_many (rho : rho) (xs : lvars) (k : rho * rho -> expr) : expr =
  match xs with
  | [] ->
      k (rho, rho)
  | x :: xs ->
      let z = apply rho x in
      dup z @@ fun (z1, z2) ->
      dup_many rho xs @@ fun (rho1, rho2) ->
      let rho1 = LVarMap.add x z1 rho1
      and rho2 = LVarMap.add x z2 rho2 in
      k (rho1, rho2)

(* -------------------------------------------------------------------------- *)

(* [transform_lvars rho xs k] transforms the list of variables [rho(xs)]
   into a list that does not have duplicate elements. This is done in
   continuation-passing style so as to allow the creation of [dup] nodes,
   if necessary, above the application of the continuation. The continuation
   [k] is applied to a list of possibly-renamed variables [zs] which does
   not have duplicate elements. *)

(* Because of the use of [List.mem], this code has quadratic complexity. *)

(* By convention, we write [x] for variables in the domain of [rho] and
   [z] for variables in its codomain. I mean, [x] for variables to which
   [rho] must be applied, and [z] for variables to which [rho] must not
   be applied. *)

let rec transform_lvars (rho : rho) (xs : lvars) (k : lvars -> expr) : expr =
  match xs with
  | [] ->
      k []
  | x :: xs ->
      let z = apply rho x in
      (* Because a renaming is injective, checking whether [rho(x)] occurs
         in [rho(xs)] is the same as checking whether [x] occurs in [xs]. *)
      if List.mem x xs then
        (* [x] appears in [xs], so a [dup] node is required. *)
        (* This node must split the variable [z] into two fresh
           variables [z1] and [z2]. *)
        dup z @@ fun (z1, z2) ->
        (* The occurrences of [x] in the list [xs] must be renamed to [z2]. *)
        let rho = LVarMap.add x z2 rho in
        transform_lvars rho xs @@ fun zs ->
        (* This occurrence of [x] is renamed to [z1]. *)
        k (z1 :: zs)
      else
        (* [x] does not appear in [xs], so no [dup] node is required. *)
        transform_lvars rho xs @@ fun zs ->
        k (z :: zs)

(* -------------------------------------------------------------------------- *)

(* [transform_expr rho e] returns an expression that is equivalent to the
   expression [rho(e)] -- that is, it has the same free variables and the same
   meaning -- and where every linear variable is used at most once. *)

(* Because of the use of [flv], this code has quadratic complexity. *)

let rec transform_expr (rho : rho) (e : expr) : expr =
  match e with

  | Loc (e, range) ->
      Loc (transform_expr rho e, range)

  | Ret (uxs, lxs) ->
      transform_lvars rho lxs @@ fun lzs ->
      Ret (uxs, lzs)

  | Let (ubs, lbs, e1, e2) ->
      (* Every variable that appears free both in [e1] and in [e2] must
         be split. *)
      let lxs = elements (inter (flv e1) (remove_lbindings lbs (flv e2))) in
      dup_many rho lxs @@ fun (rho1, rho2) ->
      Let (ubs, lbs, transform_expr rho1 e1, transform_expr rho2 e2)

  | ULiteral _ | UUnOp _ | UBinOp _ | LZero _ | UTupleIntro _ ->
      e

  | LAdd (x1, x2) ->
      transform_lvars rho [x1; x2] @@ begin function [z1; z2] ->
      LAdd (z1, z2)
      | _ -> assert false end

  | LMul (u, x) ->
      let z = apply rho x in
      LMul (u, z)

  | Drop x ->
      let z = apply rho x in
      Drop z

  | Dup x ->
      let z = apply rho x in
      Dup z

  | UTupleElim (ubs, x, e) ->
      UTupleElim (ubs, x, transform_expr rho e)

  | LTupleIntro lxs ->
      transform_lvars rho lxs @@ fun lzs ->
      LTupleIntro lzs

  | LTupleElim (lbs, x, e) ->
      let z = apply rho x in
      (* If [x] appears free in [e], then it must be split. *)
      if mem x (remove_lbindings lbs (flv e)) then
        dup z @@ fun (z1, z2) ->
        let rho = LVarMap.add x z2 rho in
        LTupleElim (lbs, z1, transform_expr rho e)
      else
        LTupleElim (lbs, z, transform_expr rho e)

  | FunCall (f, uxs, lxs) ->
      transform_lvars rho lxs @@ fun lzs ->
      FunCall (f, uxs, lzs)

(* -------------------------------------------------------------------------- *)

(* [drop x e] constructs the sequence [drop x; e]. *)

let drop x e =
  Let ([], [], Drop x, e)

(* [drop xs e] constructs the sequence [drop xs; e]. *)

let drop xs e =
  List.fold_right drop xs e

(* -------------------------------------------------------------------------- *)

(* [fix e] returns an expression that is equivalent to [e] and where every
   linear variable is used at least once. To achieve this, in every binding
   of the form [let x = e1 in e2], if [x] is not used by [e2], then a [drop]
   instruction is inserted: [let x = e1 in drop x; e2]. *)

(* Because of the use of [flv], this code has quadratic complexity. *)

let rec fix (e : expr) : expr =
  match e with
  | Loc (e, range) ->
      Loc (fix e, range)
  | Let (ubs, lbs, e1, e2) ->
      Let (ubs, lbs, fix e1, drop_fix lbs e2)
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
      -> e
  | UTupleElim (ubs, x, e2) ->
      UTupleElim (ubs, x, fix e2)
  | LTupleElim (lbs, x, e2) ->
      LTupleElim (lbs, x, drop_fix lbs e2)

and drop_fix (lbs : lbindings) (e : expr) : expr =
  (* Compute which variables must be dropped. *)
  let lxs = elements (diff (of_list (map fst lbs)) (flv e)) in
  (* Drop them, and recursively fix [e]. *)
  drop lxs (fix e)

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

(* We apply both [transform_expr] and [fix], in an arbitrary order, since these
   two transformations are independent of one another (they commute). *)

let insert_decl decl =
  clear();
  match decl with
  | Decl (range, f, ubs, lbs, e) ->
      let rho = LVarMap.empty in
      Decl (range, f, ubs, lbs, transform_expr rho (drop_fix lbs e))

let transform prog =
  fresh_names_in_namespace "k";
  List.map insert_decl prog
