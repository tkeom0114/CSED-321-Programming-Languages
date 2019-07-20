type token =
  | VID of (string)
  | CID of (string)
  | NUM of (int)
  | TRUE of (bool)
  | FALSE of (bool)
  | LPAREN
  | RPAREN
  | INT
  | BOOL
  | UNIT
  | EUNIT
  | PLUS
  | MINUS
  | PROD
  | EQ
  | LESS
  | ARROW
  | COLON
  | UNDERSCORE
  | MATCH
  | WITH
  | FUN
  | LET
  | IN
  | OF
  | REC
  | REF
  | BANG
  | COLONEQUAL
  | TYPE
  | COMMA
  | BAR
  | ENDDEC
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
# 41 "parser.ml"
let yytransl_const = [|
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* INT *);
  265 (* BOOL *);
  266 (* UNIT *);
  267 (* EUNIT *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* PROD *);
  271 (* EQ *);
  272 (* LESS *);
  273 (* ARROW *);
  274 (* COLON *);
  275 (* UNDERSCORE *);
  276 (* MATCH *);
  277 (* WITH *);
  278 (* FUN *);
  279 (* LET *);
  280 (* IN *);
  281 (* OF *);
  282 (* REC *);
  283 (* REF *);
  284 (* BANG *);
  285 (* COLONEQUAL *);
  286 (* TYPE *);
  287 (* COMMA *);
  288 (* BAR *);
  289 (* ENDDEC *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VID *);
  258 (* CID *);
  259 (* NUM *);
  260 (* TRUE *);
  261 (* FALSE *);
    0|]

let yylhs = "\255\255\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\005\000\008\000\008\000\
\007\000\009\000\009\000\006\000\006\000\010\000\010\000\001\000\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\011\000\
\011\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\003\000\001\000\001\000\001\000\001\000\003\000\003\000\002\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\003\000\003\000\003\000\001\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\004\000\001\000\002\000\002\000\
\004\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\003\000\006\000\007\000\005\000\002\000\004\000\001\000\
\002\000\003\000\005\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\056\000\057\000\052\000\053\000\054\000\000\000\
\055\000\000\000\000\000\000\000\000\000\000\000\000\000\058\000\
\000\000\030\000\000\000\000\000\048\000\000\000\000\000\000\000\
\000\000\015\000\000\000\011\000\012\000\013\000\000\000\014\000\
\010\000\000\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\000\000\
\000\000\031\000\000\000\049\000\050\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\002\000\003\000\004\000\000\000\000\000\
\000\000\047\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\033\000\000\000\045\000\000\000\
\000\000\008\000\051\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\027\000\000\000\000\000\000\000\
\020\000\024\000\000\000"

let yydgoto = "\002\000\
\016\000\022\000\079\000\034\000\091\000\023\000\081\000\092\000\
\082\000\019\000\020\000\021\000"

let yysindex = "\040\000\
\075\255\000\000\000\000\000\000\000\000\000\000\000\000\075\255\
\000\000\115\255\213\255\149\255\115\255\115\255\061\255\000\000\
\159\000\000\000\066\255\199\000\000\000\158\255\016\255\115\255\
\214\255\000\000\213\255\000\000\000\000\000\000\213\255\000\000\
\000\000\022\255\000\000\213\255\110\255\245\255\245\255\059\255\
\115\255\115\255\115\255\115\255\115\255\115\255\000\000\027\255\
\115\255\000\000\115\255\000\000\000\000\241\255\115\255\000\000\
\213\255\245\254\083\255\115\255\241\255\213\255\114\255\115\255\
\081\255\053\255\053\255\000\000\144\255\144\255\245\255\164\000\
\252\254\000\000\241\255\000\000\000\000\000\000\194\255\183\255\
\056\255\000\000\000\000\245\255\217\255\071\255\115\255\224\255\
\069\255\081\255\072\255\000\000\000\000\196\255\000\000\241\255\
\241\255\000\000\000\000\213\255\170\000\115\255\241\255\099\255\
\081\255\000\000\085\255\217\255\000\000\115\255\245\255\217\255\
\000\000\000\000\245\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\162\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\075\000\091\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\043\000\000\000\055\000\073\000\093\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\139\000\000\000\000\000\109\000\176\255\093\255\000\000\000\000\
\031\255\000\000\052\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\116\255\254\254\000\000\000\000\111\000\038\255\
\000\000\000\000\113\000"

let yygindex = "\000\000\
\000\000\005\000\195\255\246\255\037\000\065\000\125\000\035\000\
\044\000\000\000\000\000\141\000"

let yytablesize = 466
let yytable = "\085\000\
\034\000\037\000\053\000\006\000\006\000\017\000\061\000\041\000\
\042\000\043\000\044\000\045\000\006\000\094\000\025\000\006\000\
\058\000\038\000\039\000\062\000\059\000\035\000\056\000\017\000\
\046\000\063\000\055\000\006\000\006\000\006\000\006\000\017\000\
\048\000\017\000\107\000\108\000\021\000\021\000\060\000\061\000\
\001\000\112\000\036\000\022\000\022\000\066\000\067\000\068\000\
\069\000\070\000\071\000\086\000\062\000\072\000\038\000\073\000\
\015\000\023\000\023\000\080\000\021\000\040\000\021\000\021\000\
\084\000\018\000\043\000\022\000\088\000\022\000\022\000\048\000\
\039\000\065\000\040\000\003\000\004\000\005\000\006\000\007\000\
\008\000\023\000\089\000\050\000\023\000\009\000\090\000\100\000\
\061\000\083\000\041\000\101\000\042\000\103\000\010\000\015\000\
\011\000\012\000\049\000\018\000\061\000\013\000\014\000\105\000\
\015\000\113\000\111\000\018\000\025\000\018\000\043\000\098\000\
\044\000\062\000\115\000\003\000\004\000\005\000\006\000\007\000\
\024\000\007\000\007\000\018\000\064\000\009\000\104\000\061\000\
\087\000\007\000\007\000\061\000\007\000\007\000\010\000\035\000\
\011\000\012\000\026\000\114\000\062\000\013\000\014\000\109\000\
\062\000\007\000\007\000\007\000\007\000\026\000\027\000\028\000\
\029\000\030\000\031\000\041\000\042\000\043\000\047\000\032\000\
\052\000\000\000\000\000\093\000\053\000\000\000\000\000\033\000\
\016\000\041\000\042\000\043\000\044\000\045\000\036\000\054\000\
\016\000\000\000\016\000\016\000\000\000\000\000\019\000\000\000\
\000\000\000\000\046\000\000\000\055\000\099\000\019\000\000\000\
\016\000\019\000\041\000\042\000\043\000\044\000\045\000\000\000\
\095\000\000\000\106\000\000\000\000\000\000\000\019\000\096\000\
\000\000\096\000\097\000\046\000\097\000\026\000\027\000\028\000\
\029\000\030\000\031\000\000\000\098\000\000\000\098\000\032\000\
\000\000\041\000\042\000\043\000\044\000\045\000\096\000\033\000\
\000\000\097\000\057\000\041\000\042\000\043\000\044\000\045\000\
\000\000\074\000\046\000\098\000\000\000\000\000\075\000\102\000\
\076\000\077\000\078\000\000\000\046\000\000\000\000\000\000\000\
\041\000\042\000\043\000\044\000\045\000\000\000\000\000\034\000\
\000\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
\034\000\046\000\034\000\000\000\000\000\034\000\000\000\000\000\
\034\000\000\000\000\000\000\000\035\000\034\000\000\000\034\000\
\034\000\035\000\035\000\000\000\035\000\035\000\000\000\035\000\
\000\000\000\000\035\000\000\000\000\000\035\000\000\000\000\000\
\000\000\036\000\035\000\000\000\035\000\035\000\036\000\036\000\
\000\000\036\000\036\000\000\000\036\000\038\000\000\000\036\000\
\000\000\000\000\036\000\000\000\000\000\038\000\038\000\036\000\
\038\000\036\000\036\000\038\000\000\000\000\000\038\000\039\000\
\000\000\040\000\000\000\038\000\000\000\038\000\038\000\039\000\
\039\000\000\000\039\000\000\000\040\000\039\000\000\000\040\000\
\039\000\041\000\040\000\042\000\000\000\039\000\000\000\039\000\
\039\000\040\000\040\000\000\000\041\000\000\000\042\000\041\000\
\000\000\042\000\041\000\025\000\042\000\043\000\000\000\044\000\
\000\000\041\000\041\000\042\000\042\000\000\000\025\000\000\000\
\043\000\025\000\044\000\043\000\025\000\044\000\043\000\000\000\
\044\000\000\000\000\000\025\000\025\000\043\000\043\000\044\000\
\044\000\026\000\000\000\000\000\000\000\000\000\026\000\026\000\
\026\000\026\000\026\000\000\000\026\000\000\000\000\000\026\000\
\000\000\000\000\026\000\000\000\000\000\000\000\000\000\026\000\
\000\000\026\000\041\000\042\000\043\000\044\000\045\000\041\000\
\042\000\043\000\044\000\045\000\000\000\041\000\042\000\043\000\
\044\000\045\000\000\000\046\000\000\000\000\000\000\000\000\000\
\046\000\110\000\000\000\000\000\000\000\000\000\046\000\003\000\
\004\000\005\000\006\000\007\000\051\000\000\000\000\000\000\000\
\000\000\009\000"

let yycheck = "\061\000\
\000\000\012\000\007\001\006\001\007\001\001\000\018\001\012\001\
\013\001\014\001\015\001\016\001\015\001\075\000\010\000\018\001\
\027\000\013\000\014\000\031\001\031\000\000\000\007\001\007\001\
\029\001\036\000\031\001\030\001\031\001\032\001\033\001\015\001\
\006\001\017\001\096\000\097\000\006\001\007\001\017\001\018\001\
\001\000\103\000\000\000\006\001\007\001\041\000\042\000\043\000\
\044\000\045\000\046\000\062\000\031\001\049\000\000\000\051\000\
\030\001\006\001\007\001\055\000\030\001\001\001\032\001\033\001\
\060\000\001\000\014\001\030\001\064\000\032\001\033\001\006\001\
\000\000\015\001\000\000\001\001\002\001\003\001\004\001\005\001\
\006\001\030\001\002\001\019\000\033\001\011\001\006\001\032\001\
\018\001\007\001\000\000\087\000\000\000\025\001\020\001\030\001\
\022\001\023\001\033\001\007\001\018\001\027\001\028\001\032\001\
\030\001\007\001\102\000\015\001\000\000\017\001\000\000\027\001\
\000\000\031\001\110\000\001\001\002\001\003\001\004\001\005\001\
\006\001\006\001\007\001\031\001\015\001\011\001\090\000\018\001\
\015\001\014\001\015\001\018\001\017\001\018\001\020\001\011\000\
\022\001\023\001\000\000\105\000\031\001\027\001\028\001\100\000\
\031\001\030\001\031\001\032\001\033\001\001\001\002\001\003\001\
\004\001\005\001\006\001\012\001\013\001\014\001\000\000\011\001\
\020\000\255\255\255\255\000\000\007\001\255\255\255\255\019\001\
\007\001\012\001\013\001\014\001\015\001\016\001\026\001\018\001\
\015\001\255\255\017\001\018\001\255\255\255\255\007\001\255\255\
\255\255\255\255\029\001\255\255\031\001\007\001\015\001\255\255\
\031\001\018\001\012\001\013\001\014\001\015\001\016\001\255\255\
\007\001\255\255\007\001\255\255\255\255\255\255\031\001\014\001\
\255\255\014\001\017\001\029\001\017\001\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\027\001\255\255\027\001\011\001\
\255\255\012\001\013\001\014\001\015\001\016\001\014\001\019\001\
\255\255\017\001\021\001\012\001\013\001\014\001\015\001\016\001\
\255\255\001\001\029\001\027\001\255\255\255\255\006\001\024\001\
\008\001\009\001\010\001\255\255\029\001\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\007\001\
\255\255\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\029\001\018\001\255\255\255\255\021\001\255\255\255\255\
\024\001\255\255\255\255\255\255\007\001\029\001\255\255\031\001\
\032\001\012\001\013\001\255\255\015\001\016\001\255\255\018\001\
\255\255\255\255\021\001\255\255\255\255\024\001\255\255\255\255\
\255\255\007\001\029\001\255\255\031\001\032\001\012\001\013\001\
\255\255\015\001\016\001\255\255\018\001\007\001\255\255\021\001\
\255\255\255\255\024\001\255\255\255\255\015\001\016\001\029\001\
\018\001\031\001\032\001\021\001\255\255\255\255\024\001\007\001\
\255\255\007\001\255\255\029\001\255\255\031\001\032\001\015\001\
\016\001\255\255\018\001\255\255\018\001\021\001\255\255\021\001\
\024\001\007\001\024\001\007\001\255\255\029\001\255\255\031\001\
\032\001\031\001\032\001\255\255\018\001\255\255\018\001\021\001\
\255\255\021\001\024\001\007\001\024\001\007\001\255\255\007\001\
\255\255\031\001\032\001\031\001\032\001\255\255\018\001\255\255\
\018\001\021\001\018\001\021\001\024\001\021\001\024\001\255\255\
\024\001\255\255\255\255\031\001\032\001\031\001\032\001\031\001\
\032\001\007\001\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\255\255\021\001\
\255\255\255\255\024\001\255\255\255\255\255\255\255\255\029\001\
\255\255\031\001\012\001\013\001\014\001\015\001\016\001\012\001\
\013\001\014\001\015\001\016\001\255\255\012\001\013\001\014\001\
\015\001\016\001\255\255\029\001\255\255\255\255\255\255\255\255\
\029\001\024\001\255\255\255\255\255\255\255\255\029\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\255\255\011\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  INT\000\
  BOOL\000\
  UNIT\000\
  EUNIT\000\
  PLUS\000\
  MINUS\000\
  PROD\000\
  EQ\000\
  LESS\000\
  ARROW\000\
  COLON\000\
  UNDERSCORE\000\
  MATCH\000\
  WITH\000\
  FUN\000\
  LET\000\
  IN\000\
  OF\000\
  REC\000\
  REF\000\
  BANG\000\
  COLONEQUAL\000\
  TYPE\000\
  COMMA\000\
  BAR\000\
  ENDDEC\000\
  EOF\000\
  "

let yynames_block = "\
  VID\000\
  CID\000\
  NUM\000\
  TRUE\000\
  FALSE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tml.ty) in
    Obj.repr(
# 32 "parser.mly"
                            ( _2 )
# 331 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
                            ( Tml.TINT )
# 337 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
                            ( Tml.TBOOL )
# 343 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                            ( Tml.TUNIT )
# 349 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parser.mly"
                            ( Tml.TCON _1 )
# 356 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.ty) in
    Obj.repr(
# 37 "parser.mly"
                            ( Tml.TFUN (_1, _3) )
# 364 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.ty) in
    Obj.repr(
# 38 "parser.mly"
                            ( Tml.TPAIR (_1, _3) )
# 372 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tml.ty) in
    Obj.repr(
# 39 "parser.mly"
                            ( Tml.TREF (_1) )
# 379 "parser.ml"
               : Tml.ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tml.pat) in
    Obj.repr(
# 42 "parser.mly"
                            ( _2 )
# 386 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                            ( Tml.PWILD )
# 392 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                            ( Tml.PINT _1 )
# 399 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 45 "parser.mly"
                            ( Tml.PBOOL _1 )
# 406 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 46 "parser.mly"
                            ( Tml.PBOOL _1 )
# 413 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                            ( Tml.PUNIT )
# 419 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
                            ( Tml.PVAR _1 )
# 426 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
                            ( Tml.PCON _1 )
# 433 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Tml.pat) in
    Obj.repr(
# 50 "parser.mly"
                            ( Tml.PCONP (_1, _2) )
# 441 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.pat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.pat) in
    Obj.repr(
# 51 "parser.mly"
                            ( Tml.PPAIR (_1, _3) )
# 449 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.pat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.ty) in
    Obj.repr(
# 52 "parser.mly"
                            ( Tml.PTPAT (_1, _3) )
# 457 "parser.ml"
               : Tml.pat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tml.conbinding) in
    Obj.repr(
# 55 "parser.mly"
                            ( _2 )
# 464 "parser.ml"
               : Tml.conbinding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                            ( Tml.CBCON _1 )
# 471 "parser.ml"
               : Tml.conbinding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.ty) in
    Obj.repr(
# 57 "parser.mly"
                            ( Tml.CBTCON (_1, _3) )
# 479 "parser.ml"
               : Tml.conbinding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tml.conbinding) in
    Obj.repr(
# 60 "parser.mly"
                            ( [_1] )
# 486 "parser.ml"
               : 'conbind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.conbinding) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'conbind) in
    Obj.repr(
# 61 "parser.mly"
                            ( [_1] @ _3 )
# 494 "parser.ml"
               : 'conbind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.pat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 64 "parser.mly"
                            ( Tml.MRULE (_1, _3) )
# 502 "parser.ml"
               : Tml.mrule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tml.mrule) in
    Obj.repr(
# 67 "parser.mly"
                            ( [_1] )
# 509 "parser.ml"
               : 'mlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.mrule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mlist) in
    Obj.repr(
# 68 "parser.mly"
                            ( [_1] @ _3 )
# 517 "parser.ml"
               : 'mlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tml.dec) in
    Obj.repr(
# 71 "parser.mly"
                            ( _2 )
# 524 "parser.ml"
               : Tml.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'conbind) in
    Obj.repr(
# 72 "parser.mly"
                            ( Tml.DTYPE (_2, _4) )
# 532 "parser.ml"
               : Tml.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tml.dec) in
    Obj.repr(
# 75 "parser.mly"
                            ( [_1] )
# 539 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Tml.dec) in
    Obj.repr(
# 76 "parser.mly"
                            ( _1 @ [_2] )
# 547 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tml.exp) in
    Obj.repr(
# 79 "parser.mly"
                            ( ([], _1) )
# 554 "parser.ml"
               : Tml.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'declist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Tml.exp) in
    Obj.repr(
# 80 "parser.mly"
                            ( (_1, _3) )
# 562 "parser.ml"
               : Tml.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appterm) in
    Obj.repr(
# 83 "parser.mly"
                                    ( _1 )
# 569 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 84 "parser.mly"
                                    ( Tml.APP (Tml.OP(Tml.PLUS),  Tml.PAIR (_1, _3)) )
# 577 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 85 "parser.mly"
                                    ( Tml.APP (Tml.OP(Tml.MINUS), Tml.PAIR (_1, _3)) )
# 585 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 86 "parser.mly"
                                    ( Tml.APP (Tml.OP(Tml.MULT),  Tml.PAIR (_1, _3)) )
# 593 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 87 "parser.mly"
                                    ( Tml.APP (Tml.OP(Tml.EQ),    Tml.PAIR (_1, _3)) )
# 601 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 88 "parser.mly"
                                    ( Tml.APP (Tml.OP(Tml.LESS),  Tml.PAIR (_1, _3)) )
# 609 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 89 "parser.mly"
                                    ( Tml.REF _2 )
# 616 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 90 "parser.mly"
                                    ( Tml.DEREF _2 )
# 623 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 91 "parser.mly"
                                    ( Tml.ASSIGN (_1, _3) )
# 631 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Tml.pat) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 92 "parser.mly"
                                    ( Tml.LET (_2, _4, _6) )
# 640 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Tml.pat) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Tml.exp) in
    Obj.repr(
# 93 "parser.mly"
                                    ( Tml.LETREC (_3, _5, _7) )
# 649 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Tml.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Tml.ty) in
    Obj.repr(
# 94 "parser.mly"
                                    ( Tml.TEXP (_2, _4) )
# 657 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Tml.mrule) in
    Obj.repr(
# 95 "parser.mly"
                                    ( Tml.FUN _2 )
# 664 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Tml.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'mlist) in
    Obj.repr(
# 96 "parser.mly"
                                    ( Tml.MATCH (_2, _4) )
# 672 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aterm) in
    Obj.repr(
# 99 "parser.mly"
                                    ( _1 )
# 679 "parser.ml"
               : 'appterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appterm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aterm) in
    Obj.repr(
# 100 "parser.mly"
                                    ( Tml.APP (_1, _2) )
# 687 "parser.ml"
               : 'appterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tml.exp) in
    Obj.repr(
# 103 "parser.mly"
                                    ( _2 )
# 694 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Tml.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Tml.exp) in
    Obj.repr(
# 104 "parser.mly"
                                    ( Tml.PAIR (_2, _4) )
# 702 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 105 "parser.mly"
                                    ( Tml.INT _1 )
# 709 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 106 "parser.mly"
                                    ( Tml.BOOL _1 )
# 716 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 107 "parser.mly"
                                    ( Tml.BOOL _1 )
# 723 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                                    ( Tml.UNIT )
# 729 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                                    ( Tml.VAR _1 )
# 736 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                                    ( Tml.CON _1 )
# 743 "parser.ml"
               : 'aterm))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tml.program)
