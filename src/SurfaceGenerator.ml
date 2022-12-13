let length, map, filter_map, zip = List.(length, map, filter_map, combine)
open Surface

(* -------------------------------------------------------------------------- *)

(* Fresh names. *)

let fresh, clear =
  Gensym.make()

let fresh _ : var =
  Printf.sprintf "x%d" (fresh())

let ffresh, fclear =
  Gensym.make()

let ffresh _ : name =
  Printf.sprintf "f%d" (ffresh())

(* -------------------------------------------------------------------------- *)

(* Budget splitting. *)

let split_max bound budget =
  let i = Random.int (min bound budget + 1) in
  assert (i <= bound && i <= budget);
  i, budget - i

let split budget =
  split_max budget budget

(* -------------------------------------------------------------------------- *)

(* Random choices. *)

(* [choose xs] chooses a random element in the list [xs].
   The list must be nonempty. *)

let choose xs =
  let n = length xs in
  let i = Random.int n in
  List.nth xs i

(* [pick env] chooses a random entry in the environment [env].
   This environment must be nonempty. *)

let pick env : var =
  let x, ty = choose (Env.bindings env) in
  assert (ty = TReal);
  x

(* -------------------------------------------------------------------------- *)

(* Generating literals. *)

(* [literal()] picks a random literal. *)

let literals : expr list =
  List.init 5 (fun i -> lit (Real.of_int (i - 2)))
    (* -2, -1, 0, +1, +2 *)

let literal () =
  choose literals

(* -------------------------------------------------------------------------- *)

(* Generating types and function types. *)

let rec generate_type budget =
  if budget = 0 then
    TReal
  else
    (* Generating a tuple type costs 1. *)
    let budget = budget - 1 in
    TTuple (generate_types budget)

and generate_types budget =
  if budget = 0 then
    []
  else
    (* Generating an entry in a tuple type costs 1. *)
    let budget = budget - 1 in
    let budget1, budget2 = split budget in
    generate_type budget1 :: generate_types budget2

let generate_fty budget =
  let budget1, budget2 = split budget in
  generate_types budget1, generate_type budget2

(* -------------------------------------------------------------------------- *)

(* Generating expressions. *)

(* The following functions work with a conjunction of hypotheses and a
   conjunction of goals. Both are represented as environments, so
   every hypothesis or goal has a name and a type. A hypothesis or
   goal is said to be atomic when its type is [real]. *)

(* [extract_goal goals] assumes that [goals] is nonempty and extracts an
   arbitrary goal out of it. *)

let extract_goal goals =
  match Env.extract goals with
  | Some (goals, y, ty) ->
      (* We assume that every goal is atomic. *)
      assert (ty = TReal);
      goals, y
  | None ->
      (* We assume that there is at least one goal. *)
      assert false

(* If any type in the goal is a tuple type, introduce a tuple construction
   instruction (at the end of the instruction sequence).

   If any type in the environment is a tuple type, introduce a tuple
   deconstruction instruction (at the beginning of the sequence).

   If the budget is zero, try to satisfy every goal using a variable or a
   literal.

   If the budget is nonzero, introduce an arithmetic operation (in the
   beginning of the sequence) or a function call (in the middle of the
   sequence). *)

let rec generate_expr budget (fenv : fenv) (hyps : env) (goals : env) (e : expr) =
  let atomic_goals = Env.empty in
  decompose_goals budget fenv hyps goals atomic_goals e

and decompose_goals budget fenv hyps goals atomic_goals e =
  match Env.extract goals with
  | Some (goals, y, TTuple tys) ->
      (* This goal has a tuple type. *)
      (* We are expected to bind a variable [y] of type [TTuple tys]. *)
      let ys = List.map fresh tys in
      let e = Let (y, TupleIntro (vars ys), e) in
      (* The problem boils down to generating [ys : tys]. *)
      let goals = Env.bind_many goals ys tys in
      decompose_goals budget fenv hyps goals atomic_goals e
  | Some (goals, y, ty) ->
      (* This goal does not have a tuple type: it is atomic. *)
      let atomic_goals = Env.bind atomic_goals y ty in
      decompose_goals budget fenv hyps goals atomic_goals e
  | None ->
      (* Every goal is now atomic. Move on. *)
      let goals = atomic_goals in
      let atomic_hyps = Env.empty in
      decompose_hyps budget fenv hyps atomic_hyps goals e

and decompose_hyps budget fenv hyps atomic_hyps goals e =
  match Env.extract hyps with
  | Some (hyps, x, TTuple tys) ->
      (* This hypothesis has a tuple type. *)
      let xs = List.map fresh tys in
      TupleElim (xs, Var x,
      (* The hypothesis [x] is replaced with the hypotheses [xs]. *)
      let hyps = Env.bind_many hyps xs tys in
      decompose_hyps budget fenv hyps atomic_hyps goals e)
  | Some (hyps, x, ty) ->
      (* This hypothesis is atomic. *)
      let atomic_hyps = Env.bind atomic_hyps x ty in
      decompose_hyps budget fenv hyps atomic_hyps goals e
  | None ->
      (* Every hypothesis is now atomic. Move on. *)
      let hyps = atomic_hyps in
      step budget fenv hyps goals e

and step budget fenv hyps goals e =
  (* If there is no goal, then we are done, even though the budget may
     not have been fully spent. *)
  (* TODO can we ensure that the budget is not wasted? *)
  if Env.is_empty goals then
    e
  else
    (* Otherwise, choose between one of the following possibilities. *)
    let possibilities = [

      (* Allowed if there is a hypothesis, even with zero budget: *)
      not (Env.is_empty hyps),
      satisfy_goal_via_hypothesis budget fenv hyps goals e;

      (* Always allowed, even with zero budget: *)
      true,
      satisfy_goal_via_literal budget fenv hyps goals e;

      (* Allowed if the budget is nonzero: *)
      budget > 0,
      satisfy_goal_via_unary_op budget fenv hyps goals e;

      (* Allowed if the budget is nonzero: *)
      budget > 0,
      satisfy_goal_via_binary_op budget fenv hyps goals e;

      (* Allowed if there exists a function and the budget is nonzero: *)
      not (Env.is_empty fenv) && budget > 0,
      perform_function_call budget fenv hyps goals e;

    ] in
    (* Choose a possibility whose precondition holds, and activate it. *)
    let precondition_holds (pre, branch) = if pre then Some branch else None in
    choose (filter_map precondition_holds possibilities) ()

and satisfy_goal_via_hypothesis budget fenv hyps goals e () =
  let goals, y = extract_goal goals in
  let e = Let (y, Var (pick hyps), e) in
  step budget fenv hyps goals e

and satisfy_goal_via_literal budget fenv hyps goals e () =
  let goals, y = extract_goal goals in
  let e = Let (y, literal(), e) in
  step budget fenv hyps goals e

and satisfy_goal_via_unary_op budget fenv hyps goals e () =
  (* Performing a unary operation costs 1. *)
  let budget = budget - 1 in
  let goals, y = extract_goal goals in
  let op = choose [OpSin; OpCos; OpExp] in
  let x = fresh() in
  let e = Let (y, UnOp (op, Var x), e) in
  (* [x] becomes a goal. *)
  let goals = Env.bind goals x TReal in
  step budget fenv hyps goals e

and satisfy_goal_via_binary_op budget fenv hyps goals e () =
  (* Performing a binary operation costs 1. *)
  let budget = budget - 1 in
  let goals, y = extract_goal goals in
  let op = choose [OpAdd; OpSub; OpMul; OpDiv] in
  let x1, x2 = fresh(), fresh() in
  let e = Let (y, BinOp (Var x1, op, Var x2), e) in
  (* [x1] and [x2] become goals. *)
  let goals = Env.bind_many goals [x1; x2] [TReal; TReal] in
  step budget fenv hyps goals e

and perform_function_call budget fenv hyps goals e () =
  (* Performing a function call costs 1. *)
  let budget = budget - 1 in
  (* Choose a function. Decompose its type into domain and codomain. *)
  let f, fty = choose (Env.bindings fenv) in
  let tys, ty = fty in
  (* The instruction sequence that we wish to generate is split into two
     segments. In the first segment, the goals are the arguments of the
     function [f] that we have decided to invoke. In the second segment,
     the goals are unchanged, and the result of the function [f] becomes
     a new hypothesis. *)
  let xs, x = map fresh tys, fresh ty in
  let budget1, budget2 = split budget in
  let goals1 = Env.bind_many Env.empty xs tys in
  generate_expr budget1 fenv hyps goals1 (
  Let (x, FunCall (f, vars xs),
  let hyps = Env.bind hyps x ty in
  generate_expr budget2 fenv hyps goals e))

(* -------------------------------------------------------------------------- *)

(* Generating declarations. *)

let generate_decl_with_name_and_type budget fenv f fty : decl * fenv =
  clear();
  let tys, ty = fty in
  let xs, x = map fresh tys, fresh ty in
  let hyps = Env.bind_many Env.empty xs tys
  and goals = Env.bind Env.empty x ty in
  let e = generate_expr budget fenv hyps goals (Var x) in
  Decl (f, zip xs tys, e, dummy),
  Env.bind fenv f fty

let generate_decl budget fenv : decl * fenv =
  let f = ffresh() in
  (* We wish to generate function types with reasonable input and output
     arity, because testing typically requires exponential time in at
     least one dimension. *)
  let budget1, budget2 = split_max 8 budget in
  let fty = generate_fty budget1 in
  generate_decl_with_name_and_type budget2 fenv f fty

let rec generate_decls budget fenv : decls =
  if budget = 0 then
    []
  else
    (* Declaring a function costs 1. *)
    let budget = budget - 1 in
    let budget1, budget2 = split budget in
    let decl, fenv = generate_decl budget1 fenv in
    let decls = generate_decls budget2 fenv in
    decl :: decls

(* -------------------------------------------------------------------------- *)

(* Generating programs. *)

let generate budget : prog =
  fclear();
  let fenv = Env.empty in
  let prog = generate_decls budget fenv in
  (* The program should be well-typed by construction. Check anyway. *)
  ignore (SurfaceTypeChecker.infer prog);
  prog

(* TODO there is no guarantee that the call graph is connected;
        some functions may be never called; is it a problem? *)
