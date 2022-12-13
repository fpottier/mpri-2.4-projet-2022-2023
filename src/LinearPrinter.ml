open PPrint
open Linear

type highlight = {
  ranges: RangeSet.t;
  style: highlight_style;
}

and highlight_style =
  | Text
  | Ansi
  | No_highlight

let no_highlight = {
  style = No_highlight;
  ranges = RangeSet.empty;
}

let with_range highlight range doc =
  if not (RangeSet.mem range highlight.ranges)
  then doc
  else match highlight.style with
    | No_highlight ->
      doc
    | Text ->
      utf8string "(* <ERROR> *)"
      ^^ break 1
      ^^ doc
      ^^ break 1
      ^^ utf8string "(* </ERROR> *)"
    | Ansi ->
      let ansi ansi_codes = fancystring ansi_codes 0 in
      ansi Ansi.(of_styles [FG Red; Bold])
      ^^ doc
      ^^ ansi Ansi.(of_styles [Reset])

let semispace =
  utf8string "; "

(* A semicolon, followed with a space if [doc] is nonempty, followed with
   [doc]. This is a hack; in principle, pprint does not allow testing whether
   a document is empty. *)

let semi doc =
  if doc == empty then semi else semispace ^^ doc

let comma print things =
  separate_map (utf8string ", ") print things

let uvar (U x) =
  utf8string x

let lvar (L x) =
  utf8string x

let name f =
  utf8string f

let tuple print things =
  brackets (comma print things)

(* Unrestricted tuples and linear tuples are not distinguished,
   because this seems more concise. They could be distinguished
   if desired. *)

let utuple = tuple
let ltuple = tuple

let rec ty thing =
  match thing with
  | TUnknown ->
      utf8string "unknown"
  | TReal ->
      utf8string "real"
  | TTuple things ->
      tuple ty things

let ubinding (this, that) =
  uvar this ^^ utf8string " : " ^^ ty that

let lbinding (this, that) =
  lvar this ^^ utf8string " : " ^^ ty that

let mixed print1 print2 (thing1, thing2) =
  (* The semicolon delimits unrestricted things and linear things
     in a multi-value record. *)
  parens (comma print1 thing1 ^^ semi (comma print2 thing2))

let mbindings =
  mixed ubinding lbinding

let unop thing =
  match thing with
  | OpSin ->
      "sin"
  | OpCos ->
      "cos"
  | OpExp ->
      "exp"

let binop thing =
  match thing with
  | OpAdd ->
      " + "
  | OpSub ->
      " - "
  | OpMul ->
      " * "
  | OpDiv ->
      " / "

let space_group doc =
  group (nest 2 (
    break 1 ^^ doc
  ) ^^ break 1)

let def print x print1 e1 print2 e2 =
  utf8string "let " ^^ print x ^^ utf8string " =" ^^
  space_group (print1 e1) ^^
  utf8string "in" ^^ hardline ^^
  print2 e2

let rec expr hl e =
  let expr e = expr hl e in
  match e with
  | Loc (e, range) ->
      with_range hl range @@ expr e
  | Ret (uxs, lxs) ->
      mixed uvar lvar (uxs, lxs)
  | Let (ubs, lbs, e1, e2) ->
      def mbindings (ubs, lbs) expr e1 expr e2
  | ULiteral c ->
      string (Real.to_string c)
  | UUnOp (uop, x) ->
      utf8string (unop uop) ^^ parens (uvar x)
  | UBinOp (x1, bop, x2) ->
      uvar x1 ^^ utf8string (binop bop) ^^ uvar x2
  | LZero ty ->
      utf8format "0. (* %s *)" (SurfaceTypeChecker.print_type ty)
  | LAdd (x1, x2) ->
      lvar x1 ^^ utf8string " +. " ^^ lvar x2
  | LMul (x1, x2) ->
      uvar x1 ^^ utf8string " *. " ^^ lvar x2
  | Drop x ->
      utf8string "drop" ^^ parens (lvar x)
  | Dup x ->
      utf8string "dup" ^^ parens (lvar x)
  | UTupleIntro xs ->
      utuple uvar xs
  | UTupleElim (bs, x1, e2) ->
      def (utuple ubinding) bs uvar x1 expr e2
  | LTupleIntro xs ->
      ltuple lvar xs
  | LTupleElim (bs, x1, e2) ->
      def (ltuple lbinding) bs lvar x1 expr e2
  | FunCall (f, uxs, lxs) ->
      name f ^^ expr (Ret (uxs, lxs))

let decl hl d =
  match d with
  | Decl (range, f, ubs, lbs, e) ->
      with_range hl range @@
      utf8string "let " ^^ name f ^^ space ^^
      mbindings (ubs, lbs) ^^
      utf8string " =" ^^
      nest 2 (hardline ^^ expr hl e) ^^
      hardline ^^ hardline

let decls hl =
  concat_map (decl hl)

let print_program ?(highlight = no_highlight) prog =
  decls highlight prog
