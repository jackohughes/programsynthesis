type token =
  | VAR of (string)
  | NUM of (int)
  | BOOL of (bool)
  | LET
  | EQ
  | IN
  | IF
  | THEN
  | ELSE
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | GT
  | GE
  | LS
  | LE
  | NOT
  | TREE
  | CHILDREN
  | VALUE
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | COMMA
  | MAP
  | FILT
  | FOLDL
  | FOLDR
  | MAPT
  | FOLDT
  | FST
  | SND
  | FUN
  | ARROW
  | EQI
  | EQB
  | NEQI
  | NEQB
  | HEAD
  | TAIL
  | INDEX
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 54 "parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* EQ *);
  262 (* IN *);
  263 (* IF *);
  264 (* THEN *);
  265 (* ELSE *);
  266 (* ADD *);
  267 (* SUB *);
  268 (* MUL *);
  269 (* DIV *);
  270 (* MOD *);
  271 (* GT *);
  272 (* GE *);
  273 (* LS *);
  274 (* LE *);
  275 (* NOT *);
  276 (* TREE *);
  277 (* CHILDREN *);
  278 (* VALUE *);
  279 (* SEMI *);
  280 (* LPAREN *);
  281 (* RPAREN *);
  282 (* LBRACKET *);
  283 (* RBRACKET *);
  284 (* COMMA *);
  285 (* MAP *);
  286 (* FILT *);
  287 (* FOLDL *);
  288 (* FOLDR *);
  289 (* MAPT *);
  290 (* FOLDT *);
  291 (* FST *);
  292 (* SND *);
  293 (* FUN *);
  294 (* ARROW *);
  295 (* EQI *);
  296 (* EQB *);
  297 (* NEQI *);
  298 (* NEQB *);
  299 (* HEAD *);
  300 (* TAIL *);
  301 (* INDEX *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* NUM *);
  259 (* BOOL *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\002\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\006\000\006\000\008\000\008\000\009\000\009\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\000\000\000\000\000\000"

let yylen = "\002\000\
\005\000\001\000\003\000\003\000\006\000\006\000\004\000\004\000\
\004\000\005\000\005\000\005\000\006\000\006\000\006\000\001\000\
\003\000\001\000\003\000\001\000\003\000\002\000\007\000\001\000\
\001\000\001\000\002\000\003\000\001\000\005\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\026\000\025\000\024\000\000\000\
\000\000\034\000\035\000\031\000\032\000\033\000\040\000\041\000\
\042\000\043\000\044\000\049\000\048\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\051\000\000\000\
\036\000\037\000\038\000\039\000\045\000\046\000\047\000\052\000\
\000\000\016\000\029\000\000\000\053\000\000\000\054\000\000\000\
\000\000\022\000\000\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\000\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\008\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\009\000\003\000\000\000\000\000\000\000\030\000\
\000\000\000\000\010\000\012\000\000\000\000\000\011\000\000\000\
\019\000\001\000\005\000\006\000\000\000\000\000\000\000\013\000\
\014\000\015\000\000\000\023\000\000\000\000\000"

let yydgoto = "\004\000\
\053\000\045\000\063\000\064\000\041\000\081\000\042\000\054\000\
\043\000"

let yysindex = "\004\000\
\242\255\245\254\248\254\000\000\000\000\000\000\000\000\016\255\
\242\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\152\255\197\255\250\254\
\251\254\252\254\254\254\255\254\001\255\000\000\000\000\028\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\255\000\000\000\000\248\254\000\000\242\255\000\000\027\255\
\025\255\000\000\107\255\000\000\006\255\008\255\242\255\242\255\
\242\255\242\255\242\255\242\255\003\255\242\255\009\255\011\255\
\004\255\242\255\242\255\000\000\242\255\014\255\242\255\000\000\
\018\255\019\255\021\255\022\255\023\255\024\255\242\255\034\255\
\026\255\248\254\000\000\242\255\030\255\043\255\038\255\036\255\
\000\000\000\000\000\255\000\255\000\255\000\255\000\255\000\255\
\000\000\242\255\000\000\000\000\042\255\242\255\242\255\000\000\
\029\255\152\255\000\000\000\000\000\255\000\255\000\000\000\255\
\000\000\000\000\000\000\000\000\152\255\044\255\045\255\000\000\
\000\000\000\000\046\255\000\000\029\255\036\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\052\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\255\255\000\000\067\000\245\255\000\000\237\255\174\255\009\000\
\191\255"

let yytablesize = 287
let yytable = "\040\000\
\005\000\006\000\007\000\088\000\001\000\002\000\003\000\049\000\
\107\000\108\000\109\000\110\000\111\000\112\000\044\000\046\000\
\048\000\055\000\056\000\057\000\051\000\058\000\059\000\106\000\
\060\000\023\000\120\000\121\000\061\000\122\000\062\000\066\000\
\067\000\071\000\072\000\102\000\082\000\083\000\089\000\118\000\
\079\000\084\000\091\000\092\000\065\000\093\000\094\000\095\000\
\096\000\070\000\099\000\103\000\117\000\073\000\074\000\075\000\
\076\000\077\000\078\000\126\000\080\000\098\000\104\000\105\000\
\085\000\086\000\114\000\087\000\124\000\047\000\100\000\018\000\
\069\000\125\000\020\000\002\000\029\000\097\000\113\000\090\000\
\000\000\000\000\101\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\080\000\000\000\000\000\000\000\115\000\116\000\000\000\000\000\
\119\000\000\000\000\000\005\000\006\000\007\000\008\000\000\000\
\000\000\009\000\000\000\123\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\019\000\000\000\020\000\
\021\000\000\000\022\000\068\000\023\000\000\000\069\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\000\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\005\000\006\000\007\000\008\000\000\000\000\000\009\000\000\000\
\000\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\000\000\020\000\021\000\000\000\022\000\
\050\000\023\000\000\000\000\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\000\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\005\000\006\000\007\000\
\008\000\000\000\000\000\009\000\000\000\000\000\010\000\011\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\000\000\020\000\021\000\000\000\022\000\000\000\023\000\052\000\
\000\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\000\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\005\000\006\000\007\000\008\000\000\000\000\000\
\009\000\000\000\000\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\000\000\020\000\021\000\
\000\000\022\000\000\000\023\000\000\000\000\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\000\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000"

let yycheck = "\001\000\
\001\001\002\001\003\001\069\000\001\000\002\000\003\000\009\000\
\091\000\092\000\093\000\094\000\095\000\096\000\026\001\024\001\
\001\001\024\001\024\001\024\001\022\000\024\001\024\001\024\001\
\024\001\026\001\109\000\110\000\001\001\112\000\024\001\005\001\
\008\001\028\001\027\001\006\001\028\001\027\001\025\001\105\000\
\038\001\038\001\025\001\025\001\046\000\025\001\025\001\025\001\
\025\001\051\000\025\001\009\001\024\001\055\000\056\000\057\000\
\058\000\059\000\060\000\125\000\062\000\028\001\025\001\028\001\
\066\000\067\000\025\001\069\000\025\001\003\000\082\000\025\001\
\028\001\028\001\027\001\027\001\025\001\079\000\098\000\071\000\
\255\255\255\255\084\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\098\000\255\255\255\255\255\255\102\000\103\000\255\255\255\255\
\106\000\255\255\255\255\001\001\002\001\003\001\004\001\255\255\
\255\255\007\001\255\255\117\000\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\021\001\
\022\001\255\255\024\001\025\001\026\001\255\255\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\255\255\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\001\001\002\001\003\001\004\001\255\255\255\255\007\001\255\255\
\255\255\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\255\255\021\001\022\001\255\255\024\001\
\025\001\026\001\255\255\255\255\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\255\255\039\001\040\001\
\041\001\042\001\043\001\044\001\045\001\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\255\255\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\255\255\021\001\022\001\255\255\024\001\255\255\026\001\027\001\
\255\255\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\255\255\039\001\040\001\041\001\042\001\043\001\
\044\001\045\001\001\001\002\001\003\001\004\001\255\255\255\255\
\007\001\255\255\255\255\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\255\255\021\001\022\001\
\255\255\024\001\255\255\026\001\255\255\255\255\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\255\255\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001"

let yynames_const = "\
  LET\000\
  EQ\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  MOD\000\
  GT\000\
  GE\000\
  LS\000\
  LE\000\
  NOT\000\
  TREE\000\
  CHILDREN\000\
  VALUE\000\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  COMMA\000\
  MAP\000\
  FILT\000\
  FOLDL\000\
  FOLDR\000\
  MAPT\000\
  FOLDT\000\
  FST\000\
  SND\000\
  FUN\000\
  ARROW\000\
  EQI\000\
  EQB\000\
  NEQI\000\
  NEQB\000\
  HEAD\000\
  TAIL\000\
  INDEX\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  NUM\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                                       ( (_2, _4) )
# 324 "parser.ml"
               : (Ast.expr * Ast.expr)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr * Ast.expr)) in
    Obj.repr(
# 66 "parser.mly"
                   ( [_1] )
# 331 "parser.ml"
               : 'ex_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : (Ast.expr * Ast.expr)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ex_list) in
    Obj.repr(
# 67 "parser.mly"
                             ( _1 :: _3 )
# 339 "parser.ml"
               : 'ex_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ex_list) in
    Obj.repr(
# 70 "parser.mly"
                                    ( _2 )
# 346 "parser.ml"
               : (Ast.expr * Ast.expr) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                                                         ( Let (_2, _4, _6) )
# 355 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                                                       ( If (_2, _4, _6) )
# 364 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                                                  ( Lambda (_2, _4) )
# 372 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                                   ( App (_2, Empty_t, _3))
# 380 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'op) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 77 "parser.mly"
                                     ( Op (_1, _3) )
# 388 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 78 "parser.mly"
                                     ( Map (_3, _5) )
# 396 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 79 "parser.mly"
                                      ( MapT (_3, _5) )
# 404 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 80 "parser.mly"
                                      ( Filter (_3, _5) )
# 412 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 81 "parser.mly"
                                          ( FoldL (_3, _5, _6))
# 421 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 82 "parser.mly"
                                          ( FoldR (_3, _5, _6))
# 430 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 83 "parser.mly"
                                          ( FoldT (_3, _5, _6))
# 439 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 84 "parser.mly"
                                                           ( _1 )
# 446 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                               ( _2 )
# 453 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
                     ( [_1] )
# 460 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 89 "parser.mly"
                                 ( _1 :: _3 )
# 468 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 92 "parser.mly"
                      ( Cons (_1, Empty) )
# 475 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 93 "parser.mly"
                             ( Cons (_1, _3) )
# 483 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                           ( Tree.Leaf )
# 489 "parser.ml"
               : 'tree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'tree) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'tree) in
    Obj.repr(
# 97 "parser.mly"
                                               ( Tree.Node (_2, _4, _6) )
# 498 "parser.ml"
               : 'tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 100 "parser.mly"
                                                           ( Bool _1 )
# 505 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 101 "parser.mly"
                                                            ( Num _1 )
# 512 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "parser.mly"
                                                            ( Var _1 )
# 519 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                              ( List(Empty) )
# 525 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 104 "parser.mly"
                                  ( List _2 )
# 532 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tree) in
    Obj.repr(
# 105 "parser.mly"
                     ( Tree _1 )
# 539 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 106 "parser.mly"
                                       ( Pair (_2, _4) )
# 547 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
         ( Mul )
# 553 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
         ( Div )
# 559 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
         ( Mod )
# 565 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
         ( Add )
# 571 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
         ( Sub )
# 577 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
         ( Eq_Int )
# 583 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
         ( Eq_Bool )
# 589 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
         ( Neq_Int )
# 595 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
         ( Neq_Bool )
# 601 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
         ( Grt )
# 607 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
         ( Geq )
# 613 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
         ( Ls )
# 619 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
         ( LsEq )
# 625 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
        ( Not )
# 631 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
        ( Head )
# 637 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
        ( Tail )
# 643 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
         ( Index )
# 649 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
         ( Value )
# 655 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
           ( Children )
# 661 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
       ( Fst )
# 667 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
       ( Snd )
# 673 "parser.ml"
               : 'op))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry examples *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ex *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
let examples (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : (Ast.expr * Ast.expr) list)
let ex (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : (Ast.expr * Ast.expr))
