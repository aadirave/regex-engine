{ open Parser }

rule token = parse
| ['a'-'z'] as c { CHAR c }
| 'E' { EMPTY }
| '*' { STAR }
| '|' { CHOICE }
| '(' { LPAR }
| ')' { RPAR }
| eof { EOF }