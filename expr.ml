module SM = Map.Make (String)
type vars = int SM.t

type name = string

type texp =
| TArr of texp * texp
| TVar of string

type exp =
| ELam of name * texp * exp
| EApp of exp * exp
| EVar of name

type command =
| Decl of name * texp * exp

type env = exp SM.t

let rec showTExp : texp -> string = function
  | TArr (dom, cod) -> Printf.sprintf "(%s → %s)" (showTExp dom) (showTExp cod)
  | TVar value    -> value

let rec showExp : exp -> string = function
  | EVar x         -> x
  | EApp (f, x)    -> Printf.sprintf "(%s %s)" (showExp f) (showExp x)
  | ELam (x, t, y) -> Printf.sprintf "(λ (%s : %s) %s)" x (showTExp t) (showExp y)