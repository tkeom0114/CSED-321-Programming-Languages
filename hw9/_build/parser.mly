%{
%}

%token <string> VID
%token <string> CID
%token <int> NUM
%token <bool> TRUE FALSE
%token LPAREN RPAREN INT BOOL UNIT EUNIT PLUS MINUS PROD EQ LESS ARROW COLON UNDERSCORE MATCH WITH FUN LET IN OF REC REF BANG COLONEQUAL TYPE COMMA BAR ENDDEC EOF

%nonassoc IN
%nonassoc LET
%nonassoc WITH
%left     BAR
%left     COMMA
%right    ARROW
%right    COLONEQUAL
%left     EQ LESS
%left     PLUS MINUS
%left     PROD

%start program
%type <Tml.program> program
%type <Tml.exp> term
%type <Tml.ty> ty
%type <Tml.pat> pat
%type <Tml.conbinding> conbinding
%type <Tml.dec> dec
%type <Tml.mrule> mrule
%%

ty:
| LPAREN ty RPAREN          { $2 }
| INT                       { Tml.TINT }
| BOOL                      { Tml.TBOOL }
| UNIT                      { Tml.TUNIT }
| VID                       { Tml.TCON $1 }
| ty ARROW ty               { Tml.TFUN ($1, $3) }
| ty PROD ty                { Tml.TPAIR ($1, $3) }
| ty REF                    { Tml.TREF ($1) }

pat:
| LPAREN pat RPAREN         { $2 }
| UNDERSCORE                { Tml.PWILD }
| NUM                       { Tml.PINT $1 }
| TRUE                      { Tml.PBOOL $1 }
| FALSE                     { Tml.PBOOL $1 }
| EUNIT                     { Tml.PUNIT }
| VID                       { Tml.PVAR $1 }
| CID                       { Tml.PCON $1 }
| CID pat                   { Tml.PCONP ($1, $2) }
| pat COMMA pat             { Tml.PPAIR ($1, $3) }
| pat COLON ty              { Tml.PTPAT ($1, $3) }

conbinding:
| LPAREN conbinding RPAREN  { $2 }
| CID                       { Tml.CBCON $1 }
| CID OF ty                 { Tml.CBTCON ($1, $3) }

conbind:
| conbinding                { [$1] }
| conbinding BAR conbind    { [$1] @ $3 }

mrule:
| pat ARROW term            { Tml.MRULE ($1, $3) }

mlist:
| mrule                     { [$1] }
| mrule BAR mlist           { [$1] @ $3 }

dec: 
| LPAREN dec RPAREN         { $2 }
| TYPE VID EQ conbind       { Tml.DTYPE ($2, $4) }

declist:
| dec                       { [$1] }
| declist dec               { $1 @ [$2] }

program:
| term EOF                  { ([], $1) }
| declist ENDDEC term EOF   { ($1, $3) }

term:
| appterm                           { $1 }
| term PLUS term                    { Tml.APP (Tml.OP(Tml.PLUS),  Tml.PAIR ($1, $3)) }
| term MINUS term                   { Tml.APP (Tml.OP(Tml.MINUS), Tml.PAIR ($1, $3)) }
| term PROD term                    { Tml.APP (Tml.OP(Tml.MULT),  Tml.PAIR ($1, $3)) }
| term EQ term                      { Tml.APP (Tml.OP(Tml.EQ),    Tml.PAIR ($1, $3)) }
| term LESS term                    { Tml.APP (Tml.OP(Tml.LESS),  Tml.PAIR ($1, $3)) }
| REF term                          { Tml.REF $2 }
| BANG term                         { Tml.DEREF $2 }
| term COLONEQUAL term              { Tml.ASSIGN ($1, $3) }
| LET pat EQ term IN term           { Tml.LET ($2, $4, $6) }
| LET REC pat EQ term IN term       { Tml.LETREC ($3, $5, $7) }
| LPAREN term COLON ty RPAREN       { Tml.TEXP ($2, $4) }
| FUN mrule                         { Tml.FUN $2 }
| MATCH term WITH mlist             { Tml.MATCH ($2, $4) }

appterm:
| aterm                             { $1 }
| appterm aterm                     { Tml.APP ($1, $2) }

aterm:
| LPAREN term RPAREN                { $2 }
| LPAREN term COMMA term RPAREN     { Tml.PAIR ($2, $4) }                           
| NUM                               { Tml.INT $1 } 
| TRUE                              { Tml.BOOL $1 }                           
| FALSE                             { Tml.BOOL $1 } 
| EUNIT                             { Tml.UNIT } 
| VID                               { Tml.VAR $1 }
| CID                               { Tml.CON $1 }

