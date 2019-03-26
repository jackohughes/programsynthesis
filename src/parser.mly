%{
open Ast
%}

%token <string> VAR
%token <int> NUM
%token <bool> BOOL

%token LET
%token EQ
%token IN
%token IF
%token THEN
%token ELSE
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token GT
%token GE
%token LS
%token LE
%token NOT
%token TREE
%token CHILDREN
%token VALUE
%token SEMI
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token MAP
%token FILT
%token FOLDL
%token FOLDR
%token MAPT
%token FOLDT
%token FST
%token SND
%token FUN
%token ARROW
%token EQI 
%token EQB 
%token NEQI 
%token NEQB 
%token HEAD
%token TAIL
%token INDEX

%token EOF

%start expr
%start examples
%start ex
%type <Ast.expr> expr
%type <(Ast.expr * Ast.expr) list> examples
%type <(Ast.expr * Ast.expr)> ex
%%

ex:
| LPAREN expr ARROW expr RPAREN								{ ($2, $4) }

ex_list:
| ex 														{ [$1] }
| ex COMMA ex_list											{ $1 :: $3 }

examples:
| LBRACKET ex_list RBRACKET									{ $2 }

expr:
| LET VAR EQ expr IN expr;                         	    	{ Let ($2, $4, $6) }
| IF expr THEN expr ELSE expr;                   		    { If ($2, $4, $6) }
| FUN VAR ARROW expr;                        			 	{ Lambda ($2, $4) }
| LPAREN expr expr RPAREN;									{ App ($2, Empty_t, $3)}
| op LPAREN expr_list RPAREN;								{ Op ($1, $3) }
| MAP LPAREN expr RPAREN arg;								{ Map ($3, $5) }
| MAPT LPAREN expr RPAREN arg;								{ MapT ($3, $5) }
| FILT LPAREN expr RPAREN arg;								{ Filter ($3, $5) }
| FOLDL LPAREN expr RPAREN arg arg;							{ FoldL ($3, $5, $6)}
| FOLDR LPAREN expr RPAREN arg arg;							{ FoldR ($3, $5, $6)}
| FOLDT LPAREN expr RPAREN arg arg;							{ FoldT ($3, $5, $6)}
| arg;                                                    	{ $1 }
| LPAREN expr RPAREN;										{ $2 }

expr_list:
| expr 														{ [$1] }
| expr COMMA expr_list 										{ $1 :: $3 }

list:
| expr 													 	{ Cons ($1, Empty) }
| expr COMMA list 											{ Cons ($1, $3) }

tree:
| LPAREN RPAREN 											{ Tree.Leaf }
| LPAREN expr COMMA tree COMMA tree RPAREN					{ Tree.Node ($2, $4, $6) }

arg:
| BOOL                                                    	{ Bool $1 }
| NUM                                                       { Num $1 }
| VAR                                                       { Var $1 }
| LBRACKET RBRACKET											{ List(Empty) }
| LBRACKET list RBRACKET 									{ List $2 }
| tree 														{ Tree $1 }
| LPAREN expr COMMA expr RPAREN								{ Pair ($2, $4) }

op:
| MUL  		{ Mul }
| DIV  		{ Div }
| MOD  		{ Mod }
| ADD  		{ Add }
| SUB  		{ Sub }
| EQI  		{ Eq_Int }
| EQB  		{ Eq_Bool }
| NEQI 		{ Neq_Int }
| NEQB 		{ Neq_Bool }
| GT   		{ Grt }
| GE   		{ Geq } 
| LS   		{ Ls }
| LE  	 	{ LsEq }
| NOT 		{ Not }
| HEAD		{ Head }
| TAIL		{ Tail }
| INDEX		{ Index }
| VALUE		{ Value }
| CHILDREN	{ Children }
| FST		{ Fst }
| SND		{ Snd }