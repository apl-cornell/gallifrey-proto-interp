%{
open Ast
open Printf
open Lexing

let merge (fn,pos1,_) (_,_,pos2) = (fn,pos1,pos2)
%}

%token <Ast.info * int> INT
%token <Ast.info * string> VAR
%token <Ast.info> PLUS MINUS TIMES DIV MOD
  LPAREN RPAREN TRUE FALSE
  EQUALS NOTEQUALS LT LEQ GT GEQ
  NOT AND OR
  SKIP ASSIGN SEMI IF THEN ELSE WHILE DO
  LBRACE RBRACE
  PRINT DELETE COMMA COLON ARROW FUN T_INT T_BOOL
%token EOF

%nonassoc SEMI
%nonassoc THEN
%nonassoc ELSE
%right ASSIGN
%right ARROW
%right OR
%right AND
%left NOTEQUALS EQUALS 
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc DOT

%type <Ast.expr> expr
%type <Ast.expr> simple_expr
%type <(Ast.var * Ast.gt) list> varlist
%type <Ast.expr list> exprlist
%type <string * Ast.expr> field_bind
%type <(string * Ast.expr) list> fieldbindlist
%type <Ast.command> c
%type <Ast.command> p
%type <Ast.gt> type

%start p

%%

/* Expressions */
type :
| T_INT {T_int}
| T_BOOL {T_bool}

varlist :
| VAR COLON type {[(snd $1, $3)]}
| VAR COLON type COMMA varlist {(snd $1, $3)::$5}

exprlist :
| expr {[$1]}
| expr COMMA exprlist {$1::$3}

fieldbindlist : 
| field_bind {[$1]}
| field_bind COMMA fieldbindlist {$1::$3}

expr : 
  | expr AND expr { Binary(BinopAnd, $1, $3) }
  | expr OR expr { Binary(BinopOr, $1, $3) }
  | expr PLUS expr { Binary(BinopPlus, $1, $3) }
  | expr MINUS expr { Binary(BinopMinus, $1, $3) }
  | expr TIMES expr { Binary(BinopTimes, $1, $3) }
  | expr DIV expr { Binary(BinopDiv, $1, $3) }
  | expr MOD expr { Binary(BinopMod, $1, $3) }
  | expr LT expr { Binary(BinopLt, $1, $3) }
  | expr LEQ expr { Binary(BinopLeq, $1, $3) }
  | expr GT expr { Binary(BinopGt, $1, $3) }
  | expr GEQ expr { Binary(BinopGeq, $1, $3) }
  | expr NOTEQUALS expr { Binary(BinopNeq, $1, $3) }
  | expr EQUALS expr { Binary(BinopEq, $1, $3) }
  | FUN LPAREN varlist RPAREN ARROW expr 
    { Fun($3, $6) }
  | simple_expr LPAREN exprlist RPAREN {  Apply($1, $3) }
  | simple_expr { $1 } 

simple_expr : 
  | TRUE                 { Bool(true) }
  | FALSE                { Bool(false) }
  | INT                  { Int(snd $1) }
  | VAR                  { Var(snd $1) }
  | LPAREN expr RPAREN   { $2 }
  | LBRACE fieldbindlist RBRACE { Object($2) }
  | simple_expr DOT VAR { Get($1, snd $3) }

field_bind:
  | VAR COLON expr { (snd $1, $3) }

/* Commands */
c : ic SEMI c             { Seq($1, $3) }
  | ic                    { $1 }

ic: IF expr THEN ac ELSE ac  { If($2, $4, $6) }
  | WHILE expr DO ac         { While($2, $4) }
  | ac                    { $1 }

ac: SKIP { Skip }
  | simple_expr ASSIGN expr { Assign($1, $3) }

/* Programs */
p : c EOF                 { $1 }

