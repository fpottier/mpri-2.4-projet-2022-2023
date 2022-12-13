(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-39"]

open Linear
open LinearHelp
open LinearHelp.Constructors
open LVarSet
let length, map, zip = List.(length, map, combine)
let assoc xs x = List.assoc x xs

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* For each function [f] in the source program, if [f] has no unrestricted
   outputs, then we transpose [f] and define a function [tf]. *)

(* -------------------------------------------------------------------------- *)

(* Linear type environments. *)

type lenv =
  ty Env.env

let llookup (lenv : lenv) (L x : lvar) : ty =
  try Env.lookup lenv x with Not_found -> assert false

let lbind (lenv : lenv) (L x, ty : lbinding) : lenv =
  Env.bind lenv x ty

let lbind_many (lenv : lenv) (lbs : lbindings) : lenv =
  List.fold_left lbind lenv lbs

(* -------------------------------------------------------------------------- *)

(* Transposing an expression. *)

(* An expression [e] can be thought of as a data flow graph whose input wires
   are named (they are represented by free variables) and whose output wires
   are numbered (they are represented by a position in a [Ret] expression). *)

(* When transposing an expression [e], yielding a new expression [t], the
   linear inputs of [e] become linear outputs of [t], and the linear outputs
   of [e] become linear inputs of [t]. Thus, for each (named) linear input of
   [e], we must provide a number (its desired position in the output of [t]);
   and, for each (numbered) linear output of [e], we must provide a name (its
   name as an input wire of [t]). We do so as follows:

   - The list [inputs] is a list of the free linear variables of [e], in a
     certain order. (This list corresponds to Delta-dot in the paper.) It
     defines a bijection between the linear inputs of [e] and their indices.
     This list determines in what order the transposed expression [t] must
     produce its outputs.

   - The list [outputs] is a list of variables whose length must be the linear
     output arity of [e], that is, the number of linear results of [e].
     (This list corresponds to Delta-dot-dot in the paper.)
     This list assigns a name to every linear output of [e]. These names
     become the free linear variables of the transposed expression [t]. *)

(* We assume that [e] has zero unrestricted outputs. *)

(* [e] may have unrestricted inputs, but these are not a source of worry. The
   free unrestricted variables of [e] are also free unrestricted variables in
   the transposed expression. *)

(* The type environment [lenv] must map every linear variable to its type.
   It is used in the case of [Drop x]: in this case, we return [LZero ty]
   where [ty] is the type of [x]. *)

(* The function type environment [fenv] is used in an assertion in the case
   [FunCall _]. It is otherwise not needed. *)

let rec transform_expr
  (fenv : fenv) (lenv : lenv) (inputs : lvars) (outputs : lvars) (e : expr)
 : expr =
  (* The lists [inputs] and [outputs] must contain distinct names. *)
  assert (pairwise_distinct inputs);
  assert (pairwise_distinct outputs);
  (* The free linear variables of [e] must match the list [inputs]. *)
  assert (equal (of_list inputs) (flv e));
  (* The free linear variables of the transposed expression (which we
     are about to construct) must match the list [outputs]. *)
  let t : expr =
    failwith "NOT YET IMPLEMENTED"
  in
  (* The free linear variables of [t] must match the list [outputs]. *)
  assert (equal (flv t) (of_list outputs));
  t

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

(* We transpose a function [f] only if its name appears in the list [fs].
   Every function in this list must have zero unrestricted outputs. *)

(* The transposed function [tf] has the same unrestricted arguments and
   unrestricted results as the function [f]. (It has no unrestricted
   results.) Its linear arguments and linear results are those of [f],
   exchanged: linear arguments become linear results, and vice-versa.
   The order of arguments and results is preserved. *)

let transform_decl (fs : NameSet.t) fenv decl =
  clear();
  let must_transpose = NameSet.mem (name decl) fs in
  if not must_transpose then
    [decl]
  else
    let Decl (range, f, ubs, lbs, e) = decl in
    (* Look up the type of [f], and determine the number and types of
       its unrestricted and linear outputs. *)
    let utys, ltys = codomain (flookup fenv f) in
    (* [f] must have no unrestricted outputs. *)
    assert (utys = []);
    let tf = NamingConventions.transpose f in
    (* The linear inputs of [f] already have names. *)
    let inputs = map fst lbs
    and lenv = lbind_many Env.empty lbs in
    (* Invent fresh names for the linear outputs of [f]. *)
    let lbs = map lfreshb ltys in
    let outputs = map fst lbs in
    (* Keep the definition of [f] and emit the definition of [tf]. *)
    decl ::
    Decl (range, tf, ubs, lbs, transform_expr fenv lenv inputs outputs e) ::
    []

let transform fs prog =
  fresh_names_in_namespace "t";
  let fs = NameSet.of_list fs in
  let fenv = LinearTypeChecker.environment prog in
  let decls = prog in
  List.flatten (map (transform_decl fs fenv) decls)
