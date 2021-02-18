%{
  open Expr
%}

%token <string> IDENT
%token COMMA LPARENS RPARENS DEFEQ
%token LAM COLON EOF DEF ARROW

%right ARROW

%start <Expr.command list> file

%%

texp:
  | IDENT                { TVar $1       }
  | texp ARROW texp      { TArr ($1, $3) }
  | LPARENS texp RPARENS { $2 }

exp1:
  | IDENT { EVar $1 }
  | exp2 exp2 { EApp ($1, $2) }
  | LAM LPARENS IDENT COLON texp RPARENS COMMA exp1 { ELam ($3, $5, $8) }

exp2:
  | IDENT                { EVar $1 }
  | LPARENS exp1 RPARENS { $2      }

command:
  | DEF IDENT COLON texp DEFEQ exp1 { Decl ($2, $4, $6) }

file:
  | command file { $1 :: $2 }
  | EOF          { []       }