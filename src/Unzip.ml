(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-27-32-33-39"]

open Linear
open LinearHelp
open LinearHelp.Constructors
open LinearHelp.Contexts

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* Each function [f] in the original program is transformed into two
   functions, [uf] and [lf]. *)

(* The function [uf] takes just the unrestricted parameters of [f] and
   returns its unrestricted results plus a number of unrestricted
   auxiliary results. *)

(* The function [lf] takes the auxiliary results of [uf] plus the
   linear parameters of [f] and returns the linear results of [f]. *)

open NamingConventions

(* -------------------------------------------------------------------------- *)

(* Types. *)

(* The type of [uf] depends not only on the type of [f], but also on the
   body of the function [f]. *)

(* Because we do not have recursive functions, when we unzip a call of a
   function [f], we have already unzipped the definition of [f]. This allows
   us to look up (in a suitable environment) the type of the translated
   function [uf]. (This is noted at the end of Section 4 in the paper.) *)

(* An environment [env] is a pair [ifenv, ofenv]. *)

(* [ifenv] maps every function [f] in the original program to its type. *)

(* [ofenv] maps every function [uf] in the unzipped program to its type. *)

type env =
  fenv * fenv

(* -------------------------------------------------------------------------- *)

(* Expressions. *)

(* The environment [env] records the type of every source function [f] and of
   every transformed function [uf]. It does not evolve when [transform_expr]
   recursively calls itself. *)

let rec transform_expr (env : env) (e : expr) : uctx * expr * expr =
  failwith "NOT YET IMPLEMENTED"

(* -------------------------------------------------------------------------- *)

(* Declarations. *)

(* While declarations are processed, [ifenv] is fixed, and [ofenv] grows. *)

(* A function [f] is unzipped only if its name occurs in [fs].  *)

let transform_decl (fs : NameSet.t) (ifenv : fenv) (ofenv : fenv) (decl : decl)
: fenv * decls =
  clear();
  let must_unzip = NameSet.mem (name decl) fs in
  if not must_unzip then
    ofenv, [decl]
  else
    let Decl (range, f, ubs, lbs, e) = decl in
    let uf, lf, cf = unrestricted f, linear f, combined f in
    (* Construct the definitions of the functions [uf], [lf], [cf]. *)
    let udef, ldef, cdef =
      failwith "NOT YET IMPLEMENTED"
    in
    (* Extend [ofenv] with the type of [uf]. *)
    let ufty = failwith "NOT YET IMPLEMENTED" in
    let ofenv = fbind ofenv uf ufty in
    (* Done. *)
    ofenv, [udef; ldef; cdef]

(* -------------------------------------------------------------------------- *)

(* Programs. *)

let transform (fs : names) (prog : prog) : prog =
  fresh_names_in_namespace "u";
  let fs = NameSet.of_list fs in
  let decls = prog in
  let ifenv = LinearTypeChecker.environment decls in
  let ofenv = Env.empty in
  let _ofenv, decls = List.fold_left_map (transform_decl fs ifenv) ofenv decls in
  List.flatten decls
