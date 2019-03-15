%{
%}

%token <int> INT
%token <string> ID
%token TRUE FALSE PLUS MINUS STAR SLASH EQUAL LESS AND OR NOT LPAREN RPAREN
%token SKIP COLONEQ SEMICOLON IF THEN ELSE FI WHILE DO END LBRACE RBRACE EOF

%left SEMICOLON
%left OR
%left AND
%right NOT
%nonassoc EQUAL LESS
%left PLUS MINUS
%left STAR SLASH

%start program
%type <Imp.program> program
%%

program:
    | stmt SEMICOLON expr EOF   { ($1,$3) }
    ;

expr:
    | LPAREN expr RPAREN    { $2 }
    | INT                   { Imp.Num $1 }
    | TRUE                  { Imp.Bool true }
    | FALSE                 { Imp.Bool false }
    | ID                    { Imp.Var $1 }
    | NOT expr              { Imp.Not $2 }
    | expr PLUS  expr       { Imp.Add($1,$3) }
    | expr MINUS expr       { Imp.Sub($1,$3) }
    | expr STAR  expr       { Imp.Mul($1,$3) }
    | expr SLASH expr       { Imp.Div($1,$3) }
    | expr EQUAL expr       { Imp.Eq($1,$3) }
    | expr LESS  expr       { Imp.Less($1,$3) }
    | expr AND   expr       { Imp.And($1,$3) }
    | expr OR    expr       { Imp.Or($1,$3) }
    ;

stmt:
    | LBRACE stmt RBRACE                { $2 }
    | SKIP                              { Imp.Skip }
    | ID COLONEQ expr                   { Imp.Assign($1,$3) }
    | stmt SEMICOLON stmt               { Imp.Seq($1,$3) }
    | IF expr THEN stmt ELSE stmt FI    { Imp.If($2,$4,$6) }
    | IF expr THEN stmt FI              { Imp.If2($2,$4) }
    | WHILE expr DO stmt END            { Imp.While($2,$4) }
    ;
%%
