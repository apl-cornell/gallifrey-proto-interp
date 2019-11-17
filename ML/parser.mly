%{
open Ast
open Printf
open Lexing
open Utils

exception DuplicateField

let merge (fn,pos1,_) (_,_,pos2) = (fn,pos1,pos2)

let dedup_field t_obj = 
  let sorted = List.sort_uniq (fun (x,_,_,_) (y,_,_,_) -> String.compare x y) t_obj in
  if List.length sorted <> List.length t_obj then raise DuplicateField
  else t_obj

let dedup_obj_field t_obj = 
  let sorted = List.sort_uniq (fun (x,_,_,_,_) (y,_,_,_,_) -> String.compare x y) t_obj in
  if List.length sorted <> List.length t_obj then raise DuplicateField
  else t_obj
%}

%token <Ast.info * int> INT
%token <Ast.info * string> VAR CVAR
%token <Ast.info> PLUS MINUS TIMES DIV MOD
  LPAREN RPAREN TRUE FALSE
  EQUALS NOTEQUALS LT LEQ GT GEQ
  NOT AND OR
  SKIP ASSIGN SEMI IF THEN ELSE WHILE DO
  LBRACE RBRACE
  PRINT COMMA COLON DOT ARROW LAMBDA T_INT T_BOOL T_UNIT
  BRANCH FOCUS U MUT SLEEP UNIT LET IN DESTROY CLASS EXTENDS CAPOF
%token EOF

%nonassoc IN
%left SEMI
%nonassoc THEN
%nonassoc ELSE
%left RPAREN
%right LPAREN
%right ASSIGN
%right ARROW
%right OR
%right AND
%nonassoc NOT
%left NOTEQUALS EQUALS 
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc DOT

%type <Ast.expr> expr
%type <Ast.expr> p
%type <Ast.param list> lambdalist
%type <Ast.param list> biglambda
%type <Ast.param list> biglambdalist
%type <Ast.expr list> exprlist
%type <Ast.gtype> type
%type <Ast.gtype list > typelist
%type <string list> varlist


%start p

%%

/* Expressions */
type :
| T_INT {T_int}
| T_BOOL {T_bool}
| biglambdalist ARROW type { T_fun(Utils.normalize $1, $3) }
| T_UNIT {T_unit}
| CVAR {T_cls(snd $1)}

typelist :
| type {[$1]}
| type COMMA typelist {$1 :: $3}

varlist:
| VAR {[snd $1]}
| VAR COMMA varlist {(snd $1)::$3}

lambdalist :
| VAR COLON VAR type { [Lambda(snd $1, snd $3, $4)]}
| VAR COLON VAR type COMMA lambdalist { Lambda(snd $1, snd $3, $4)::$6 }

biglambda :
| LPAREN VAR COLON VAR CVAR OR lambdalist RPAREN { (SigmaLambda(snd $2, snd $4, T_cls(snd $5)))::$7 }
| LPAREN VAR COLON VAR CVAR OR RPAREN { [SigmaLambda(snd $2, snd $4, T_cls(snd $5))] }
| LPAREN OR lambdalist RPAREN { $3 }
| LPAREN VAR OR RPAREN { [KappaLambda(snd $2)] }
| LPAREN VAR OR lambdalist RPAREN { (KappaLambda(snd $2))::$4 }

biglambdalist : 
| biglambda { $1 }
| biglambda biglambdalist { ($1)@($2) }

fieldlist :
| VAR COLON type {[(snd $1, $3, A, IMMUT)]}
| MUT VAR COLON type {[(snd $2, $4, A, MUT)]}
| VAR COLON type COMMA fieldlist {(snd $1, $3, A, IMMUT)::$5}
| MUT VAR COLON type COMMA fieldlist {(snd $2, $4, A, MUT)::$6}
| U VAR COLON type {[(snd $2, $4, U, IMMUT)]}
| MUT U VAR COLON type {[(snd $3, $5, U, MUT)]}
| U VAR COLON type COMMA fieldlist {(snd $2, $4, U, IMMUT)::$6}
| MUT U VAR COLON type COMMA fieldlist {(snd $3, $5, U, MUT)::$7}

exprlist :
| expr {[$1]}
| expr COMMA exprlist {$1::$3}

expr : 
  | MINUS expr { Neg($2) }
  | NOT expr { Not($2) }
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
  | LAMBDA biglambdalist ARROW type LBRACE expr RBRACE { Fun(Utils.normalize $2, $4, $6) }
  | expr LPAREN exprlist RPAREN {  Apply($1, $3) }
  | CVAR LPAREN exprlist RPAREN {  Apply(Var(snd $1), $3) }
  | DESTROY LPAREN expr RPAREN {  Destroy($3) }
  | SLEEP LPAREN expr RPAREN {  Sleep($3) }
  | CAPOF LPAREN expr RPAREN {  Capof($3) }
  | TRUE                 { Bool(true) }
  | FALSE                { Bool(false) }
  | INT                  { Int(snd $1) }
  | VAR                  { Var(snd $1) }
  | LPAREN expr RPAREN   { $2 }
  | expr DOT VAR { Get($1, snd $3) }
  | expr SEMI expr             { Seq($1, $3) }
  | IF expr LBRACE expr RBRACE ELSE LBRACE expr RBRACE  { If($2, $4, $8) }
  | WHILE expr LBRACE expr RBRACE     { While($2, $4) }
  | FOCUS VAR LBRACE expr RBRACE     { Focus(snd $2, $4) }
  | BRANCH varlist LBRACE expr RBRACE     { Branch($2, $4) }
  | LET VAR ASSIGN expr IN expr { Let(snd $2, $4, $6) }
  | expr ASSIGN expr { Assign($1, $3) }
  | CLASS CVAR LBRACE fieldlist RBRACE { Class(snd $2, dedup_field $4, None) }
  | CLASS CVAR EXTENDS CVAR LBRACE fieldlist RBRACE { Class(snd $2, dedup_field $6, Some (snd $4)) }
  
/* Programs */
p : expr EOF                 { $1 }

