open PPrint
open Surface

let var =
  utf8string

let name =
  utf8string

let brack =
  brackets

let comma print things =
  separate_map (utf8string ", ") print things

let tuple print things =
  brack (comma print things)

let rec ty thing =
  match thing with
  | TUnknown ->
      utf8string "unknown"
  | TReal ->
      utf8string "real"
  | TTuple things ->
      tuple ty things

let binding (this, that) =
  var this ^^ utf8string " : " ^^ ty that

let bindings =
  comma binding

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
      "+"
  | OpSub ->
      "-"
  | OpMul ->
      "*"
  | OpDiv ->
      "/"

let space_group doc =
  group (nest 2 (
    break 1 ^^ doc ^^ break 1
  ))

let def print x print1 e1 print2 e2 =
  utf8string "let " ^^ print x ^^ utf8string " =" ^^
  space_group (print1 e1) ^^
  utf8string "in" ^^ hardline ^^
  print2 e2

let rec expr e =
  match e with
  | Loc (e, _range) ->
      expr e
  | Let (x, e1, e2) ->
      def var x expr e1 expr e2
  | TupleElim (xs, e1, e2) ->
      def (tuple var) xs expr e1 expr e2
  | _ ->
      additive_expr e

and additive_expr e =
  match e with
  | Loc (e, _range) ->
      additive_expr e
  | BinOp (e1, ((OpAdd | OpSub) as bop), e2) ->
      additive_expr e1 ^^ space ^^
      utf8string (binop bop) ^^
      group (break 1 ^^ multiplicative_expr e2)
  | _ ->
      multiplicative_expr e

and multiplicative_expr e =
  match e with
  | Loc (e, _range) ->
      multiplicative_expr e
  | BinOp (e1, ((OpMul | OpDiv) as bop), e2) ->
      multiplicative_expr e1 ^^ space ^^
      utf8string (binop bop) ^^
      group (break 1 ^^ atomic_expr e2)
  | _ ->
      atomic_expr e

and atomic_expr e =
  match e with
  | Loc (e, _range) ->
      atomic_expr e
  | Var x ->
      var x
  | Literal c ->
      string (Real.to_string c)
  | TupleIntro es ->
      tuple expr es
  | FunCall (f, es) ->
      name f ^^ parens (comma expr es)
  | UnOp (uop, e) ->
      utf8string (unop uop) ^^ parens (expr e)
  | _ ->
      parens (expr e)

let decl (Decl (f, bs, e, _range)) =
  utf8string "let " ^^ name f ^^ space ^^
  parens (bindings bs) ^^ utf8string " =" ^^
  nest 2 (hardline ^^ expr e) ^^
  hardline ^^ hardline

let decls =
  concat_map decl

let print_program =
  decls

let print_expr =
  expr
