open Surface

(* -------------------------------------------------------------------------- *)

(* [occurrences x e] counts the number of free occurrences of the
   variable [x] in the expression [e]. *)

let rec occurrences (x : var) (e : expr) : int =
  match e with
  | Loc (e, _) ->
      occurrences x e
  | Var y ->
      if x = y then 1 else 0
  | Literal _ ->
      0
  | UnOp (_, e) ->
      occurrences x e
  | BinOp (e1, _, e2) ->
      occurrences x e1 + occurrences x e2
  | Let (y, e1, e2) ->
      occurrences x e1 +
      if x = y then 0 else occurrences x e2
  | TupleIntro es ->
      sum_occurrences x es
  | TupleElim (xs, e1, e2) ->
      occurrences x e1 +
      if List.mem x xs then 0 else occurrences x e2
  | FunCall (_, es) ->
      sum_occurrences x es

and sum_occurrences x es =
  match es with
  | [] ->
      0
  | e :: es ->
      occurrences x e + sum_occurrences x es

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

type env =
  expr Env.env

let transform_var (env : env) (x : var) : expr =
  try
    Env.lookup env x
  with Not_found ->
    Var x

let rec transform_expr (env : env) (e : expr) : expr =
  match e with
  | Loc (e, range) ->
      Loc (transform_expr env e, range)
  | Var x ->
      transform_var env x
  | Literal _ ->
      e
  | UnOp (op, e) ->
      UnOp (op, transform_expr env e)
  | BinOp (e1, op, e2) ->
      BinOp (transform_expr env e1, op, transform_expr env e2)
  | Let (x, e1, e2) ->
      let e1 = transform_expr env e1 in
      let env = Env.remove env x in
      (* Calling [occurrences] here results in quadratic complexity.
         This is inefficient, and one could do better, but we do not
         care. *)
      let n = occurrences x e2 in
      if n = 0 then
        (* If [x] is not used in [e2], drop its definition. *)
        transform_expr env e2
      else if n = 1 then
        (* If [x] is used only once in [e2], replace it with its definition. *)
        let env = Env.bind env x e1 in
        transform_expr env e2
      else
        (* Otherwise, keep [x]. *)
        Let (x, e1, transform_expr env e2)
  | TupleIntro es ->
      TupleIntro (transform_exprs env es)
  | TupleElim (xs, e1, e2) ->
      let e1 = transform_expr env e1 in
      let env = List.fold_left Env.remove env xs in
      TupleElim (xs, e1, transform_expr env e2)
  | FunCall (f, es) ->
      FunCall (f, transform_exprs env es)

and transform_exprs env es =
  List.map (transform_expr env) es

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

let transform_decl decl =
  match decl with
  | Decl (f, bs, e, range) ->
      let env = Env.empty in
      Decl (f, bs, transform_expr env e, range)

let transform prog =
  List.map transform_decl prog
