%{
  open Expr
%}

%token <string> IDENT
%token COMMA LPARENS RPARENS DEFEQ
%token LAM COLON EOF DEF ARROW ABBREV

%right ARROW

%start <Expr.command list> file

%%

params:
  | IDENT params { $1 :: $2 }
  |              { []       }

texp:
  | IDENT params         { TVar ($1, $2) }
  | texp ARROW texp      { TArr ($1, $3) }
  | LPARENS texp RPARENS { $2            }

tele:
  | LPARENS IDENT COLON texp RPARENS { (Ident $2, $4) }

cotele:
  | tele cotele { $1 :: $2 }
  | tele        { [$1]     }

exp1:
  | IDENT                 { EVar (Ident $1) }
  | exp2 exp2             { EApp ($1, $2)   }
  | LAM cotele COMMA exp1 { lam $2 $4       }

exp2:
  | IDENT                { EVar (Ident $1) }
  | LPARENS exp1 RPARENS { $2              }

command:
  | ABBREV IDENT params DEFEQ texp  { Abbrev ($2, $3, $5) }
  | DEF IDENT COLON texp DEFEQ exp1 { Decl ($2, $4, $6)   }

file:
  | command file { $1 :: $2 }
  | EOF          { []       }