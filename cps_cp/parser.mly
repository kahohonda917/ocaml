%{
(* 補助的な変数、関数、型などの定義 *)
let make_fun vars expr =
  List.fold_right (fun v e -> Syntax.Fun (v, e)) vars expr
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE
%token EQUAL NOTEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token <string> VAR
%token TRUE FALSE
%token IF THEN ELSE LET REC IN FUN ARROW
%token TRY WITH SHIFT RESET CONTROL PROMPT
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc ELSE IN ARROW WITH
%nonassoc EQUAL NOTEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
        { Syntax.Number ($1) }
| VAR
        { Syntax.Var ($1) }
| LPAREN expr RPAREN
        { $2 }

expr:
| simple_expr
        { $1 }
| expr PLUS expr
        { Syntax.Plus ($1, $3) }
| FUN VAR ARROW expr
        { Syntax.Fun ($2, $4) }
| app
        { $1 }

| CONTROL VAR ARROW expr
        { Syntax.Control ($2, $4) }
| PROMPT simple_expr
        { Syntax.Prompt ($2) }

/* vars: */
/* | */
/*         { [] } */
/* | VAR vars */
/*         { $1 :: $2 } */

app:
| simple_expr simple_expr
        { Syntax.App ($1, $2) }
| app simple_expr
        { Syntax.App ($1, $2) }
