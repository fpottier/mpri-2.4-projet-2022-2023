open Linear
open LinearHelp

(* We insert the prefix "s" in front of every identifier; see LinearHelp. *)

let u (x : string) : uvar =
  U ("s" ^ x)

let transform_ubinding (x, ty) : ubinding =
  u x, ty

let transform_ubinding_unknown x : ubinding =
  transform_ubinding (unknown x)

let rec transform_expr (e : Surface.expr) : expr =
  match e with
  | Surface.Loc (e, _range) ->
      transform_expr e
  | Surface.Var x ->
      uvar (u x)
  | Surface.Literal c ->
      ULiteral c
  | Surface.UnOp (uop, e) ->
      with_uvar (transform_expr e) @@ fun x ->
      UUnOp (uop, x)
  | Surface.BinOp (e1, bop, e2) ->
      with_uvar (transform_expr e1) @@ fun x1 ->
      with_uvar (transform_expr e2) @@ fun x2 ->
      UBinOp (x1, bop, x2)
  | Surface.Let (x, e1, e2) ->
      Let ([transform_ubinding_unknown x], [], transform_expr e1, transform_expr e2)
  | Surface.TupleIntro es ->
      with_uvars (map transform_expr es) @@ fun xs ->
      UTupleIntro xs
  | Surface.TupleElim (xs, e1, e2) ->
      with_uvar (transform_expr e1) @@ fun x1 ->
      UTupleElim (map transform_ubinding_unknown xs, x1, transform_expr e2)
  | Surface.FunCall (f, es) ->
      with_uvars (map transform_expr es) @@ fun xs ->
      FunCall (f, xs, [])

let transform_decl (decl : Surface.decl) : decl =
  clear();
  match decl with
  | Surface.Decl (f, bs, e, range) ->
      Decl (Source range, f, map transform_ubinding bs, [], transform_expr e)

let transform (prog : Surface.prog) : prog =
  fresh_names_in_namespace "s";
  map transform_decl prog
