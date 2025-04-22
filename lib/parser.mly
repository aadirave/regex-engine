%token <char> CHAR
%token EMPTY STAR CHOICE LPAR RPAR CONCAT
%token EOF

%nonassoc LPAR EMPTY CHAR

%left CHOICE
%left STAR
%left CONCAT

%start main
%type <Ast.regex_t> main 
%type <Ast.regex_t> regex

%%

// Ast is also known in all actions below
main: r = regex EOF { r } 

regex:
| EMPTY { Empty }
| c = CHAR { Char c }
| LPAR r = regex RPAR { r }
| a = regex CHOICE b = regex { Choice(a, b) }
| r = regex STAR { Star r }
| a = regex b = regex %prec CONCAT { Concat(a, b) }
