open Printf
open Linear
open LinearHelp

let map, length, iter, iter2, zip, unzip =
  List.(map, length, iter, iter2, combine, split)

(* -------------------------------------------------------------------------- *)

(* The exception [Error] carries a range and a message. It is normally raised
   with a dummy range. (This removes the need to propagate range information
   down to the place where the exception is raised.) This dummy range can be
   replaced with a better range as the exception propagates upwards and
   traverses an invocation of [with_range]. *)

exception Error of range option * string

(* [fail format ...] raises [Error] without a range. *)

let fail format =
  ksprintf (fun msg ->
    raise (Error (None, msg))
  ) format

(* [with_range range f x] computes [f x]. If an [Error] exception is raised,
   then this exception is allowed to propagate upwards. If error has no
   range, then the propagated error uses [range]. *)

let with_range range f x =
  try
    f x
  with Error (None, msg) ->
    raise (Error (Some range, msg))

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions on types. *)

(* [known ty] checks that the type [ty] is fully known, that is, [unknown]
   does not appear in it. *)

let rec known (ty : ty) : unit =
  match ty with
  | TUnknown ->
      fail "The type `unknown` appears in a type annotation"
  | TReal ->
      ()
  | TTuple tys ->
      iter known tys

(* [conforms ty1 ty2] checks that the inferred type [ty2] conforms to the
   declared type [ty1]. This means that [ty2] must be more defined than [ty1].
   That is, they must be equal, except in places where [ty1] is [unknown]. The
   inferred type [t2] is not allowed to contain [unknown]. *)

let rec conforms (ty1 : ty) (ty2 : ty) : unit =
  match ty1, ty2 with
  | _, TUnknown ->
      (* [ty2] is not allowed to contain [unknown]. *)
      assert false
  | TUnknown, _ ->
      ()
  | TReal, TReal ->
      ()
  | TTuple tys1, TTuple tys2 ->
      if length tys1 <> length tys2 then
        fail "Expected a tuple of arity %d; got a tuple of arity %d"
          (length tys1) (length tys2);
      iter2 conforms tys1 tys2
  | TReal, TTuple tys2 ->
      fail "Expected a real; got a tuple of arity %d" (length tys2)
  | TTuple tys1, TReal ->
      fail "Expected a tuple of arity %d; got a real" (length tys1)

let arity (mvty : mvty) : int * int =
  let utys, ltys = mvty in
  length utys, length ltys

let arity_conforms (expected : mvty) (inferred : mvty) =
  let uexpected, lexpected = arity expected
  and uinferred, linferred = arity inferred in
  if uexpected <> uinferred then
    fail "Expected %d unrestricted wires; got %d unrestricted wires"
      uexpected uinferred;
  if lexpected <> linferred then
    fail "Expected %d linear wires; got %d linear wires"
      lexpected linferred

(* [mvconforms] is analogous to [conforms], but operates on multi-value types. *)

let mvconforms (mvty1 : mvty) (mvty2 : mvty) : unit =
  arity_conforms mvty1 mvty2;
  let utys1, ltys1 = mvty1
  and utys2, ltys2 = mvty2 in
  iter2 conforms utys1 utys2;
  iter2 conforms ltys1 ltys2

(* [untuple] projects away a [TTuple] constructor. *)

let untuple (ty : ty) : tys =
  match ty with
  | TTuple tys ->
      tys
  | TUnknown
  | TReal ->
      assert false

(* -------------------------------------------------------------------------- *)

(* Generic environments. *)

let empty =
  Env.empty

let lookup kind env x =
  try
    Env.lookup env x
  with Not_found ->
    fail "Unbound %s: %s" kind x

let bind env x ty =
  known ty;
  Env.bind env x ty

let check_distinct kind xs =
  try
    Env.check_distinct xs
  with Env.Collision x ->
    fail "Multiply-defined %s: %s" kind x

let bind_many kind env xs tys =
  check_distinct kind xs;
  List.fold_left2 bind env xs tys

(* -------------------------------------------------------------------------- *)

(* Environments for functions. *)

let flookup fenv f =
  lookup "function" fenv f

(* -------------------------------------------------------------------------- *)

(* Environments for unrestricted variables. *)

type uenv =
  ty Env.env

let uproj (U x) =
  x

let ulookup uenv x =
  lookup "unrestricted variable" uenv (uproj x)

let ubind_many uenv xs tys =
  bind_many "unrestricted variable" uenv (map uproj xs) tys

(* [ubindb1_many] extends the environment [uenv] with several bindings. *)

let ubindb1_many uenv (bs : ubindings) : uenv =
  let xs, tys = unzip bs in
  iter known tys;
  ubind_many uenv xs tys

(* [ubindb2_many uenv bs inferred] extends the environment [uenv] with new
   bindings. The bindings [bs] carry declared types, and we also have a list
   of [inferred] types. We check that they match, and we keep the inferred
   types, which are more defined. We return a pair of an updated environment
   and updated bindings. *)

let ubindb2_many uenv (bs : ubindings) (inferred : tys) : uenv * ubindings =
  assert (length bs = length inferred);
  let xs, declared = unzip bs in
  iter2 conforms declared inferred;
  let tys = inferred in
  let uenv = ubind_many uenv xs tys
  and bs = zip xs tys in
  uenv, bs

(* -------------------------------------------------------------------------- *)

(* Environments for linear variables. *)

type lenv =
  ty Env.env

let lproj (L x) =
  x

let llookup lenv x =
  lookup "linear variable" lenv (lproj x)

let lbind_many lenv xs tys =
  bind_many "linear variable" lenv (map lproj xs) tys

let lbindb1_many lenv (bs : lbindings) : lenv =
  let xs, tys = unzip bs in
  iter known tys;
  lbind_many lenv xs tys

let lbindb2_many lenv (bs : lbindings) (inferred : tys) : lenv * lbindings =
  assert (length bs = length inferred);
  let xs, declared = unzip bs in
  iter2 conforms declared inferred;
  let tys = inferred in
  let lenv = lbind_many lenv xs tys
  and bs = zip xs tys in
  lenv, bs

(* -------------------------------------------------------------------------- *)

(* Pairing the previous two kinds of environments. *)

type env =
  uenv * lenv

let bindb2_many (env : env) (bs : bindings) (inferred : mvty) : env * bindings =
  let uenv, lenv = env
  and ubs, lbs = bs
  and uinferred, linferred = inferred in
  let declared : mvty = map snd ubs, map snd lbs in
  arity_conforms declared inferred;
  let uenv, ubs = ubindb2_many uenv ubs uinferred
  and lenv, lbs = lbindb2_many lenv lbs linferred in
  let env = uenv, lenv
  and bs = ubs, lbs in
  env, bs

(* -------------------------------------------------------------------------- *)

(* Type-checking expressions. *)

(* [infer fenv env e] infers the multi-value type of the expression [e].
   If [e] is ill-formed or ill-typed, it raises [Error].
   It returns a pair of the inferred type and an updated expression where all
   occurrences of the type [TUnknown] have been replaced with inferred types. *)

let rec infer (fenv : fenv) (env : env) (e : expr) : mvty * expr =
  match e with
  | Loc (e, range) ->
    with_range range (infer fenv env) e

  | Ret (uxs, lxs) ->
      let uenv, lenv = env in
      (map (ulookup uenv) uxs, map (llookup lenv) lxs),
      e

  | Let (ubs, lbs, e1, e2) ->
      let bs = ubs, lbs in
      let mvty1, e1 = infer fenv env e1 in
      let env, bs = bindb2_many env bs mvty1 in
      let mvty2, e2 = infer fenv env e2 in
      let ubs, lbs = bs in
      mvty2,
      Let (ubs, lbs, e1, e2)

  | ULiteral _ ->
      u1 real,
      e

  | UUnOp (_, x) ->
      let uenv, _ = env in
      conforms real (ulookup uenv x);
      u1 real,
      e

  | UBinOp (x1, _, x2) ->
      let uenv, _ = env in
      conforms real (ulookup uenv x1);
      conforms real (ulookup uenv x2);
      u1 real,
      e

  | LZero ty ->
      (* Zero is polymorphic. *)
      l1 ty,
      e

  | LAdd (x1, x2) ->
      (* Addition is polymorphic. *)
      let _, lenv = env in
      let ty1 = llookup lenv x1
      and ty2 = llookup lenv x2 in
      conforms ty1 ty2; (* an equality check *)
      l1 ty1,
      e

  | LMul (x1, x2) ->
      (* Multiplication is not polymorphic, although it could be, if needed. *)
      let uenv, lenv = env in
      conforms (ulookup uenv x1) real;
      conforms (llookup lenv x2) real;
      l1 real,
      e

  | Drop x ->
      let _, lenv = env in
      let _ty = llookup lenv x in
      ([], []),
      e

  | Dup x ->
      let _, lenv = env in
      let ty = llookup lenv x in
      l2 ty ty,
      e

  | UTupleIntro uxs ->
      let uenv, _ = env in
      let tys = map (ulookup uenv) uxs in
      u1 (TTuple tys),
      e

  | UTupleElim (ubs, x1, e2) ->
      let uenv, lenv = env in
      let ty1 = ulookup uenv x1 in
      let declared = TTuple (map snd ubs) in
      conforms declared ty1;
      let uenv, ubs = ubindb2_many uenv ubs (untuple ty1) in
      let env = uenv, lenv in
      let mvty2, e2 = infer fenv env e2 in
      mvty2,
      UTupleElim (ubs, x1, e2)

  | LTupleIntro lxs ->
      let _, lenv = env in
      let tys = map (llookup lenv) lxs in
      l1 (TTuple tys),
      e

  | LTupleElim (lbs, x1, e2) ->
      let uenv, lenv = env in
      let ty1 = llookup lenv x1 in
      let declared = TTuple (map snd lbs) in
      conforms declared ty1;
      let lenv, lbs = lbindb2_many lenv lbs (untuple ty1) in
      let env = uenv, lenv in
      let mvty2, e2 = infer fenv env e2 in
      mvty2,
      LTupleElim (lbs, x1, e2)

  | FunCall (f, uxs, lxs) ->
      let domain, codomain = flookup fenv f in
      let inferred, _ = infer fenv env (Ret (uxs, lxs)) in
      mvconforms domain inferred;
      codomain,
      e

(* -------------------------------------------------------------------------- *)

(* Type-checking declarations. *)

let check_decl fenv decl : fenv * decl =
  match decl with
  | Decl (range, f, ubs, lbs, e) ->
      let domain : mvty = map snd ubs, map snd lbs in
      with_range range (fun () ->
        let uenv = ubindb1_many empty ubs
        and lenv = lbindb1_many empty lbs in
        let env = uenv, lenv in
        let codomain, e = infer fenv env e in
        let fty = domain, codomain in
        let fenv = fbind fenv f fty in
        fenv,
        Decl (range, f, ubs, lbs, e)
      ) ()

let check_decls =
  List.fold_left_map check_decl

let name_of decl =
  match decl with
  | Decl (_, f, _, _, _) ->
      f

let check_prog (prog : prog) : fenv * prog =
  check_distinct "function" (map name_of prog);
  let fenv = empty in
  check_decls fenv prog

let check (prog : prog) : prog =
  let _fenv, decls = check_prog prog in
  decls

let environment (prog : prog) : fenv =
  let fenv, _decls = check_prog prog in
  fenv

(* -------------------------------------------------------------------------- *)

(* Enforcing linearity. *)

include struct

  open LVarSet

  (* The following auxiliary functions cause a failure if a variable
     is used twice. *)

  let used_twice (x : lvar) =
    fail "The linear variable %s is used more than once" (lproj x)

  let add (x : lvar) (xs : t) : t =
    if mem x xs then used_twice x else add x xs

  let of_list (xs : lvars) : t =
    List.fold_right add xs empty

  let (+) xs1 xs2 =
    match choose (inter xs1 xs2) with
    | exception Not_found -> xs1 + xs2
    | x -> used_twice x

  (* The following auxiliary functions cause a failure if a variable
     is not used. *)

  let not_used (x : lvar) =
    fail "The linear variable %s is never used" (lproj x)

  let remove (x : lvar) (xs : t) : t =
    if mem x xs then remove x xs else not_used x

  let remove_lbinding ((x, _) : lbinding) (s : t) : t =
    remove x s

  let remove_lbindings (lbs : lbindings) (s : t) : t =
    List.fold_right remove_lbinding lbs s

  (* The following function is apparently identical to [LinearHelp.flv] but
     relies on a different set of auxiliary functions. If a linear variable is
     never used or used twice, then this is detected and causes a failure. *)

  let rec flv (e : expr) : t =
    match e with
    | Loc (e, range) ->
      with_range range flv e
    | UTupleElim (_, _, e) ->
        flv e
    | Ret (_, lxs)
    | LTupleIntro lxs
    | FunCall (_, _, lxs) ->
        of_list lxs
    | Let (_, lbs, e1, e2) ->
        flv e1 + remove_lbindings lbs (flv e2)
    | ULiteral _
    | UUnOp _
    | UBinOp _
    | LZero _
    | UTupleIntro _ ->
        empty
    | LAdd (x1, x2) ->
        of_list [x1; x2]
    | LMul (_, x)
    | Drop x
    | Dup x ->
        singleton x
    | LTupleElim (lbs, x, e) ->
        singleton x + remove_lbindings lbs (flv e)

  let check_linearity decl =
    match decl with
    | Decl (range, _, _, lbs, e) ->
        with_range range (fun e ->
          let xs = remove_lbindings lbs (flv e) in
          assert (is_empty xs)
        ) e

  let check_linearity decls =
    List.iter check_linearity decls

end

(* -------------------------------------------------------------------------- *)

(* Combine type-checking and linearity checking. *)

let check mode prog =
  let prog = check prog in
  match mode with
  | `Lenient ->
      prog
  | `Strict ->
      check_linearity prog;
      prog
