type token =
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQUAL
  | NOTEQUAL
  | LESS
  | LESSEQUAL
  | GREATER
  | GREATEREQUAL
  | NUMBER of (int)
  | VAR of (string)
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | LET
  | REC
  | IN
  | FUN
  | ARROW
  | TRY
  | WITH
  | SHIFT
  | RESET
  | CONTROL
  | PROMPT
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
