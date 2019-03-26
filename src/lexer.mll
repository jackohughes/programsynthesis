
{  open Parser 
   exception Eof
   }

let white = [' ' '\t' '\n' '\r']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*

rule token = parse
 | white                 { token lexbuf }
 | "(*"                  { comment lexbuf } 
 | "fun"				 { FUN }
 | "map"				 { MAP }
 | "foldr"				 { FOLDR }
 | "foldl" 				 { FOLDL }
 | "filt" 				 { FILT }
 | "mapt" 				 { MAPT }
 | "foldt"  			 { FOLDT }
 | "fst" 				 { FST }
 | "snd" 				 { SND }
 | "let"                 { LET }
 | "in"                  { IN }
 | "if"                  { IF }
 | "then"                { THEN }
 | "else"                { ELSE }
 | "not"                 { NOT }
 | "tree"                { TREE }
 | "children"            { CHILDREN }
 | "value"               { VALUE }
 | "+"                   { ADD }
 | "-"                   { SUB }
 | "*"                   { MUL }
 | "/"                   { DIV }
 | "%"                   { MOD }
 | "="					 { EQ }
 | "=="					 { EQI }
 | "==."				 { EQB }
 | "!="					 { NEQI }
 | "!=."				 { NEQB }
 | ">"                   { GT }
 | ">="                  { GE }
 | "<"                   { LS }
 | "<="                  { LE }
 | "hd"					 { HEAD }
 | "tl"					 { TAIL }
 | "index"				 { INDEX }
 | ";"                   { SEMI }
 | "->"					 { ARROW }
 | "true"                { BOOL true }
 | "false"               { BOOL false }
 | '('                   { LPAREN }
 | ')'                   { RPAREN }
 | '['                   { LBRACKET }
 | ']'                   { RBRACKET }
 | ','                   { COMMA }
 | "eof"                 { failwith "end" }
 | id as text            { VAR text }
 | '-'?['0'-'9']+ as num { NUM (int_of_string num) }
 | _                     { failwith (Lexing.lexeme lexbuf) }
and comment = parse
 | "*)"                  { token lexbuf }
 | _                     { comment lexbuf }