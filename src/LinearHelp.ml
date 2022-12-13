open Linear

(* -------------------------------------------------------------------------- *)

(* List functions that we use without a [List.] qualifier. *)

let length, map =
  List.(length, map)

(* -------------------------------------------------------------------------- *)

(* Sugar for certain constructions. *)

(* The type [real]. *)

let real =
  TReal

(* One unrestricted wire. *)

let u1 x =
  [x], []

(* Two unrestricted wires. *)

let u2 x1 x2 =
  [x1; x2], []

(* One linear wire. *)

let l1 x =
  [], [x]

(* Two linear wires. *)

let l2 x1 x2 =
  [], [x1; x2]

(* Converting variables to expressions. *)

let uvar (x : uvar) : expr =
  Ret ([x], [])

let lvar (x : lvar) : expr =
  Ret ([], [x])

(* Converting variables to bindings, carrying an [unknown] type. *)

let unknown x =
  (x, TUnknown)

(* -------------------------------------------------------------------------- *)

(* An (arbitrary) ordering on unrestricted variables. *)

module UVar = struct
  type t = uvar
  let compare (U x1) (U x2) = String.compare x1 x2
end

(* Sets of unrestricted variables. *)

module UVarSet = struct

  include Set.Make(UVar)

  let (+) = union

  let reduce f xs =
    List.fold_left (fun s x -> union s (f x)) empty xs

  let remove_ubinding ((x, _) : ubinding) (s : t) : t =
    remove x s

  let remove_ubindings (ubs : ubindings) (s : t) : t =
    List.fold_right remove_ubinding ubs s

  let pairwise_distinct (xs : uvars) =
    length xs = cardinal (of_list xs)

end

(* Maps over unrestricted variables. *)

module UVarMap = Map.Make(UVar)

(* An (arbitrary) ordering on linear variables. *)

module LVar = struct
  type t = lvar
  let compare (L x1) (L x2) = String.compare x1 x2
end

(* Sets of linear variables. *)

module LVarSet = struct

  include Set.Make(LVar)

  let (+) = union

  let reduce f xs =
    List.fold_left (fun s x -> union s (f x)) empty xs

  let remove_lbinding ((x, _) : lbinding) (s : t) : t =
    remove x s

  let remove_lbindings (lbs : lbindings) (s : t) : t =
    List.fold_right remove_lbinding lbs s

  let pairwise_distinct (xs : lvars) =
    length xs = cardinal (of_list xs)

end

(* Maps over linear variables. *)

module LVarMap = Map.Make(LVar)

(* -------------------------------------------------------------------------- *)

(* Computing the free variables of an expression. *)

include struct

  open UVarSet

  (**[fuv e] is the set of free unrestricted variables of
     the expression [e]. *)
  let rec fuv (e : expr) : t =
    match e with
    | Loc (e, _)
    | LTupleElim (_, _, e) ->
        fuv e
    | Ret (uxs, _)
    | UTupleIntro uxs
    | FunCall (_, uxs, _) ->
        of_list uxs
    | Let (ubs, _, e1, e2) ->
        fuv e1 + remove_ubindings ubs (fuv e2)
    | ULiteral _
    | LZero _
    | LTupleIntro _
    | LAdd _
    | Drop _
    | Dup _ ->
        empty
    | UUnOp (_, x)
    | LMul (x, _) ->
        singleton x
    | UBinOp (x1, _, x2) ->
        of_list [x1; x2]
    | UTupleElim (ubs, x, e) ->
        singleton x + remove_ubindings ubs (fuv e)

end

include struct

  open LVarSet

  let rec flv (e : expr) : t =
    match e with
    | Loc (e, _)
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

end

(* -------------------------------------------------------------------------- *)

(* Generating fresh variables. *)

(* We must ensure that a generated name cannot collide with a name that
   already exists in the program. And, since the program is transformed in
   several successive phases, we must ensure that a generated name cannot
   collide with any name that was generated in an earlier phase.

   Regarding local variables, we propose to divide the universe into several
   disjoint namespaces, identified by incomparable prefixes, and to let each
   transformation "own" a distinct namespace, as follows:

   * Surface2Linear, which imports a surface program into Linear, owns the
     namespace "s". It prepends this prefix in front of every identifier: as
     a result, every variable inhabits this namespace, and every other
     namespace is uninhabited.

   * Freshen owns the namespace "h".

   * Normalize generates no fresh names.

   * Simplify generates no fresh names.

   * ForwardMode owns the namespace "l", where it generates fresh names,
     and the namespace "d", where it allocates names in a deterministic
     way: e.g., an existing name "sx" is mapped to "dsx".

   * DupDropInsertion owns the namespace "k".

   * Unzip owns the namespace "u".

   * Transpose owns the namespace "t".

   * Forget owns the namespace "z".

   The global functions [ufresh] and [lfresh] are used to generate fresh
   names. The global function [fresh_names_in_namespace] determines the
   namespace used by [ufresh] and [lfresh]. (The use of global state may
   be frowned upon, but seems more convenient than carrying an extra
   namespace parameter through many auxiliary functions.) The global
   function [no_fresh_names] forbids the use of [ufresh] and [lfresh].

   Within its private namespace, each transformation allocates fresh
   names using counters, which are managed by the functions [ufresh]
   and [lfresh]. The function [clear] resets both counters. [clear()]
   can typically be called at the beginning of each Linear function,
   since all Linear variables are local. *)

let current_namespace =
  ref None

let fresh_names_in_namespace s =
  current_namespace := Some s

let no_fresh_names () =
  current_namespace := None

let current_namespace () =
  match !current_namespace with
  | None ->
      (* Attempt to generate fresh names without first choosing a namespace! *)
      assert false
  | Some s ->
      s

let ufresh, uclear =
  Gensym.make()

let ufresh () : uvar =
  U (Printf.sprintf "%su%d" (current_namespace()) (ufresh()))

let ufreshb (ty : ty) : ubinding =
  ufresh(), ty

let lfresh, lclear =
  Gensym.make()

let lfresh () : lvar =
  L (Printf.sprintf "%sl%d" (current_namespace()) (lfresh()))

let lfreshb (ty : ty) : lbinding =
  lfresh(), ty

let clear () =
  uclear();
  lclear()

(* -------------------------------------------------------------------------- *)

(* Generating fresh variables that stand for a subexpression. *)

(* [with_uvar e k] applies the continuation [k] to a variable that stands for
   the expression [e]. This variable can be [e] itself, it this expression is
   just a variable, or a freshly generated variable, in which case the result
   of the continuation is wrapped in a [let] binding. *)

(* The expression [e] must have one unrestricted output wire and no linear
   output wire. *)

let rec with_uvar (e : expr) (k : uvar -> expr) : expr =
  match e with
  | Loc (e, _) ->
      with_uvar e k
  | Ret (uxs, lxs) ->
      assert (length uxs = 1);
      assert (length lxs = 0);
      let x = List.hd uxs in
      (* [e] is already an unrestricted variable. *)
      k x
  | _ ->
      (* Generate a fresh variable [x] to stand for [e]. *)
      let x = ufresh() in
      Let ([unknown x], [], e, k x)

(* [with_lvar] is analogous, but concerns linear variables. *)

(* The expression [e] must have no unrestricted output wire and one linear
   output wire. *)

let rec with_lvar (e : expr) (k : lvar -> expr) : expr =
  match e with
  | Loc (e, _) ->
      with_lvar e k
  | Ret (uxs, lxs) ->
      assert (length uxs = 0);
      assert (length lxs = 1);
      let x = List.hd lxs in
      (* [e] is already a linear variable. *)
      k x
  | _ ->
      (* Generate a fresh variable [x] to stand for [e]. *)
      let x = lfresh() in
      Let ([], [unknown x], e, k x)

(* The following two functions are iterated versions of the previous two. *)

let rec with_uvars (es : exprs) (k : uvars -> expr) : expr =
  match es with
  | [] ->
      k []
  | e :: es ->
      with_uvar e @@ fun x ->
      with_uvars es @@ fun xs ->
      k (x :: xs)

let rec with_lvars (es : exprs) (k : lvars -> expr) : expr =
  match es with
  | [] ->
      k []
  | e :: es ->
      with_lvar e @@ fun x ->
      with_lvars es @@ fun xs ->
      k (x :: xs)

(* -------------------------------------------------------------------------- *)

(* Smart constructors. *)

module Constructors = struct

  (* Literals. *)

  let one : expr =
    ULiteral Real.one

  let minus_one : expr =
    ULiteral Real.minus_one

  (* Unrestricted operations. *)

  let cos e =
    with_uvar e @@ fun x ->
    UUnOp (OpCos, x)

  let sin e =
    with_uvar e @@ fun x ->
    UUnOp (OpSin, x)

  let exp e =
    with_uvar e @@ fun x ->
    UUnOp (OpExp, x)

  let ( * ) e1 e2 =
    with_uvar e1 @@ fun x1 ->
    with_uvar e2 @@ fun x2 ->
    UBinOp (x1, OpMul, x2)

  let ( / ) e1 e2 =
    with_uvar e1 @@ fun x1 ->
    with_uvar e2 @@ fun x2 ->
    UBinOp (x1, OpDiv, x2)

  let uminus e =
    minus_one * e

  (* Linear operations. *)

  let ( +. ) e1 e2 =
    with_lvar e1 @@ fun dx1 ->
    with_lvar e2 @@ fun dx2 ->
    LAdd (dx1, dx2)

  let ( *. ) e1 e2 =
    with_uvar e1 @@ fun x1 ->
    with_lvar e2 @@ fun dx2 ->
    LMul (x1, dx2)

  let lminus e =
    minus_one *. e

  let ( -. ) e1 e2 =
    e1 +. lminus e2

  (**Assuming [e1] has one unrestricted output and [e2] has one linear output,
     [pair e1 e2] is a combined expression that has both outputs. It is a form
     of parallel composition of two boxes. *)
  let pair (e1 : expr) (e2 : expr) =
    with_uvar e1 @@ fun x1 ->
    with_lvar e2 @@ fun dx2 ->
    Ret ([x1], [dx2])

  (**[uwiden e extra] extends the box [e] with the unrestricted output wires
     represented by the variables [extra]. The expression [e] is currently
     expected to be either [Ret _] or an expression with one unrestricted
     output wire. Other forms of expressions could be supported if needed. *)
  let rec uwiden (e : expr) (extra : uvars) =
    match e, extra with
    | _, [] ->
        e
    | Ret (uxs, lxs), _ ->
        Ret (uxs @ extra, lxs)
    | (ULiteral _ | UUnOp _ | UBinOp _ | UTupleIntro _), _ ->
        (* [e] has arity 1. Introduce one variable to name its output wire,
           then put all output wires together. *)
        with_uvar e @@ fun x ->
        Ret (x :: extra, [])
    | Loc (e, _), _ ->
        uwiden e extra
    | _, _ ->
        (* Unexpected form of expression. *)
        assert false

  (**[lwiden outputs1 outputs2 e] assumes that the linear output wires of the
     expression [e] match the list [outputs1], in this order, and produces a
     "widened" expression whose linear output wires match the list [outputs2],
     in this order. The names in the list [outputs1] must form a subset of the
     names in the list [outputs2]. If the two sets of names coincide (that is,
     if the two lists have the same length) then this is just a shuffling. If
     the list [outputs1] is shorter than the list [outputs2], then every name
     in [outputs2 \ outputs1] becomes an input and an output of the widened
     box. *)
  let lwiden (outputs1 : lvars) (outputs2 : lvars) (e : expr) : expr =
    assert LVarSet.(subset (of_list outputs1) (of_list outputs2));
    (* As a special case, if the lists [outputs1] and [outputs2] coincide,
       then no [let] binding is necessary. *)
    if outputs1 = outputs2 then
      e
    else
      Let ([], map unknown outputs1, e, Ret ([], outputs2))

end

(* -------------------------------------------------------------------------- *)

(* Contexts. *)

module Contexts : sig

  (**[uctx] is an abstract type of contexts that bind unrestricted
     variables only. *)
  type uctx

  (**The empty context. *)
  val empty: uctx

  (**Context concatenation. [c1 ++ c2] is the context [c1[c2[]]].
     The contexts [c1] and [c2] must bind disjoint sets of variables,
     so that no shadowing takes place: a variable bound in [c2] cannot
     hide a variable bound in [c1]. *)
  val (++): uctx -> uctx -> uctx

  (**[fill c e] fills the context [c] with an expression [e],
     producing an expression. *)
  val fill: uctx -> expr -> expr

  (**[bindings c] is the list of (unrestricted) variables bound by
     the context [c]. *)
  val bindings: uctx -> ubindings

  (**[ulet ubs e1] is the context [let ubs = e1 in []]. *)
  val ulet: ubindings -> expr -> uctx

  (**[utuple_elim ubs x1] is the context [let (ubs;) = x1 in []]. *)
  val utuple_elim: ubindings -> uvar -> uctx

end = struct

  let disjoint (xs1 : uvars) (xs2 : uvars) : bool =
    UVarSet.(disjoint (of_list xs1) (of_list xs2))

  let disjoint (ubs1 : ubindings) (ubs2 : ubindings) : bool =
    disjoint (map fst ubs1) (map fst ubs2)

  type uctx =
  | Leaf of { ubs: ubindings; fill: expr -> expr }
  | Cat of uctx * uctx

  let rec bindings c (k : ubindings) : ubindings =
    match c with
    | Leaf { ubs; _ } ->
        ubs @ k
    | Cat (c1, c2) ->
        bindings c1 (bindings c2 k)

  let bindings c =
    bindings c []

  let empty =
    Leaf { ubs = []; fill = fun e -> e }

  let (++) c1 c2 =
    (* A sanity check. Not necessary in principle. Bad complexity. *)
    assert (disjoint (bindings c1) (bindings c2));
    Cat (c1, c2)

  let rec fill c e =
    match c with
    | Leaf { ubs = _; fill } ->
        fill e
    | Cat (c1, c2) ->
        fill c1 (fill c2 e)

  let ulet ubs e1 =
    let fill e2 = Let (ubs, [], e1, e2) in
    Leaf { ubs; fill }

  let utuple_elim ubs x1 =
    let fill e2 = UTupleElim (ubs, x1, e2) in
    Leaf { ubs; fill }

end

(* -------------------------------------------------------------------------- *)

(* Renamings. *)

module Renamings = struct

  (* A renaming is a (total) function of unrestricted variables to
     unrestricted variables and of linear variables to linear variables. *)

  type rho =
    uvar UVarMap.t * lvar LVarMap.t

  (* The identity renaming. *)

  let identity : rho =
    (UVarMap.empty, LVarMap.empty)

  (* Applying a renaming to a variable. *)

  let uapply (urho, _ : rho) (x : uvar) : uvar =
    try UVarMap.find x urho with Not_found -> x

  let lapply (_, lrho : rho) (x : lvar) : lvar =
    try LVarMap.find x lrho with Not_found -> x

  (* [extend rho ubs uxs lbs lxs] extends the renaming [rho] by mapping [ubs]
     to [uys] and [lbs] to [lys]. *)

  let extend (urho, lrho : rho) ubs uys lbs lys =
    let uxs = map fst ubs
    and lxs = map fst lbs in
    List.fold_right2 UVarMap.add uxs uys urho,
    List.fold_right2 LVarMap.add lxs lys lrho

  (* [compose rho ubs uys lbs lys] composes the renaming [rho] with the
     renaming that maps [ubs] to [uys] and [lbs] to [lys]. *)

  let compose rho ubs uys lbs lys =
    extend rho
      ubs (map (uapply rho) uys)
      lbs (map (lapply rho) lys)

  (* [restrict rho ubs lbs] restricts the renaming [rho] by mapping [ubs]
     to itself and [lbs] to itself. *)

  let restrict (urho, lrho : rho) ubs lbs =
    let uxs = map fst ubs
    and lxs = map fst lbs in
    List.fold_right UVarMap.remove uxs urho,
    List.fold_right LVarMap.remove lxs lrho

end

(* -------------------------------------------------------------------------- *)

(* Annotating programs with virtual locations for error printing. *)

(* [annotate e] annotates each node of the AST [e] with a distinct
   virtual location. These locations can then be used for precise
   printing of type error locations. *)

let rec annotate (e : expr) : expr =
  let range = next_virtual_range () in
  (fun e -> Loc (e, range)) @@ match e with
  | Loc (e, _range) ->
    (* We only use the annotated expression for
       pinpointing type checking errors, so we
       don't need to preserve source locations. *)
      annotate e
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
  | Let (ubs, lbs, e1, e2) ->
      let e1' = annotate e1 in
      let e2' = annotate e2 in
      Let (ubs, lbs, e1', e2')
  | UTupleElim (ubs, x, e) ->
      UTupleElim (ubs, x, annotate e)
  | LTupleElim (lbs, l, e) ->
      LTupleElim (lbs, l, annotate e)

let annotate_decl (Decl (range, f, ubs, xbs, e)) =
  Decl (range, f, ubs, xbs, annotate e)

let annotate_prog decls = List.map annotate_decl decls
