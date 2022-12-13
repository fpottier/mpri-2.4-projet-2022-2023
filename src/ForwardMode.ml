(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-39"]

open Linear
open LinearHelp
open LinearHelp.Constructors

(* We assume that the source program does not use any linear variables. *)

(* We assume that every multi-result in the source program has arity 1. *)

(* The paper assumes that every variable is used at least once. This applies
   even to nonlinear variables: see the first paragraph of Section 2.2.
   Because of this assumption, the transformation J (Figure 8) does not
   need to introduce any drops. We do not impose this restriction, so we
   can produce code where some [drop] instructions are missing. *)

(* If the source expression uses an unrestricted variable [x : tau] then
   the transformed expression uses both an unrestricted variable [x : tau]
   and a linear variable [dx : tau]. If the source expression produces an
   unrestricted result of type [tau] then (in the same position) the
   transformed expression produces both an unrestricted result of type
   [tau] and a linear result of type [tau]. *)

(* For simplicity, the mapping of the name [x] to the name [dx] is fixed.
   Collisions are avoided by assuming that the name [x] never begins with
   the character [d]. *)

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

open NamingConventions

let dot (U x : uvar) : lvar =
  assert (String.length x > 0 && x.[0] <> 'd');
  L ("d" ^ x)

let dots (xs : uvars) : lvars =
  map dot xs

let transform_var (x : uvar) : expr =
  lvar (dot x)

let transform_binding (x, ty : ubinding) : lbinding =
  let dx = dot x in
  dx, ty

let transform_bindings (ubs : ubindings) : lbindings =
  map transform_binding ubs

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

let rec transform_expr (e : expr) : expr =
  failwith "NOT YET IMPLEMENTED"

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

let transform_decl (decl : decl) : decl =
  clear();
  match decl with
  | Decl (range, f, ubs, lbs, e) ->
      assert (lbs = []);
      let df = derivative f
      and lbs = transform_bindings ubs in
      Decl (range, df, ubs, lbs, transform_expr e)

let transform prog =
  fresh_names_in_namespace "l";
  List.fold_right (fun decl decls ->
    decl :: transform_decl decl :: decls
  ) prog []
