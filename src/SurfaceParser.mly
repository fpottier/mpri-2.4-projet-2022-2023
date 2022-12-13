%token <float> FLOAT
%token <string> IDENTIFIER
%token REAL
%token LET EQUAL IN
%token SIN COS EXP
%token PLUS MINUS TIMES DIV
%token COMMA COLON
%token LPAREN RPAREN
%token LBRACK RBRACK
%token EOF
%start <Surface.prog> prog
%{ open Surface %}

%%

let located(E) ==
  e = E;
    { let range = ($startpos, $endpos) in Loc (e, range) }

let comma(X) ==
  separated_list(COMMA, X)

let paren(X) ==
  delimited(LPAREN, X, RPAREN)

let brack(X) ==
  delimited(LBRACK, X, RBRACK)

let name ==
  IDENTIFIER

let var ==
  IDENTIFIER

let ty :=
  | REAL;
      { TReal }
  | tys = brack(comma(ty));
      { TTuple tys }

let binding ==
  separated_pair(var, COLON, ty)

let bindings ==
  comma(binding)

let unop :=
  | SIN;
      { OpSin }
  | COS;
      { OpCos }
  | EXP;
      { OpExp }

let additive_binop :=
   | PLUS;
       { OpAdd }
   | MINUS;
       { OpSub }

let multiplicative_binop :=
  | TIMES;
      { OpMul }
  | DIV;
      { OpDiv }

let expr :=
  located(
    | LET; x = var; EQUAL; e1 = expr; IN; e2 = expr;
        { Let (x, e1, e2) }
    | LET; xs = brack(comma(var)); EQUAL; e1 = expr; IN; e2 = expr;
        { TupleElim (xs, e1, e2) }
  )
  | additive_expr

let additive_expr :=
  located (
    | e1 = additive_expr; bop = additive_binop; e2 = multiplicative_expr;
        { BinOp (e1, bop, e2) }
  )
  | multiplicative_expr

let multiplicative_expr :=
  located(
    | e1 = multiplicative_expr; bop = multiplicative_binop; e2 = atomic_expr;
        { BinOp (e1, bop, e2) }
  )
  | atomic_expr

let atomic_expr :=
  located (
    | x = var;
        { Var x }
    | c = FLOAT;
        { Literal (Real.of_float c) }
    | es = brack(comma(expr));
        { TupleIntro es }
    | f = name; es = paren(comma(expr));
        { FunCall (f, es) }
    | uop = unop; e = paren(expr);
        { UnOp (uop, e) }
  )
  | paren(expr)

let decl :=
  LET; f = name; bs = paren(bindings); EQUAL; e = expr;
    { let range = ($startpos, $endpos) in Decl (f, bs, e, range) }

let prog :=
  terminated(decl*, EOF)
