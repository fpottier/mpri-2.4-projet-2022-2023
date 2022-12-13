{

  open Printf
  open SurfaceParser

  exception Error of string

  let fail lexbuf format =
    ksprintf (fun s ->
      let s = sprintf "At offset %d: %s.\n" (Lexing.lexeme_start lexbuf) s in
      raise (Error s)
    ) format

}

let digit =
  ['0'-'9']

rule token = parse
| [' ' '\t']+
    { token lexbuf }
| '\n'
    { MenhirLib.LexerUtil.newline lexbuf; token lexbuf }
| (['-' '+']? digit+ ('.' digit+)?) as f
    { try FLOAT (float_of_string f)
      with Failure _ -> fail lexbuf "invalid floating-point literal" }
| "real"
    { REAL }
| "let"
    { LET }
| '='
    { EQUAL }
| "in"
    { IN }
| "sin"
    { SIN }
| "cos"
    { COS }
| "exp"
    { EXP }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| ','
    { COMMA }
| ':'
    { COLON }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '['
    { LBRACK }
| ']'
    { RBRACK }
| (['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*) as s
    { IDENTIFIER s }
| eof
    { EOF }
| "(*"
    { comment lexbuf; token lexbuf }
| _
    { fail lexbuf "unexpected character" }

and comment = parse
| "(*"
    { comment lexbuf; comment lexbuf }
| "*)"
    { () }
| _
    { comment lexbuf }
