module SM = Map.Make (String)
type vars = int SM.t

type name =
| Ident of string
| Index of int

type texp =
| TArr of texp * texp
| TVar of string * string list

type exp =
| ELam of name * texp * exp
| EApp of exp * exp
| EVar of name

type command =
| Abbrev of string * string list * texp
| Decl   of string * texp * exp
| Eval   of texp * exp

type env = exp SM.t

let rec showTExp : texp -> string = function
  | TArr (dom, cod)      -> Printf.sprintf "(%s → %s)" (showTExp dom) (showTExp cod)
  | TVar (value, [])     -> value
  | TVar (value, params) ->
    String.concat " " params
    |> Printf.sprintf "(%s %s)" value

let showName : name -> string = function
  | Ident x -> x
  | Index x -> Printf.sprintf "?%d" x

let rec showExp : exp -> string = function
  | ELam (x, t, y) -> Printf.sprintf "(λ (%s : %s) %s)" (showName x) (showTExp t) (showExp y)
  | EApp (f, x)    -> Printf.sprintf "(%s %s)" (showExp f) (showExp x)
  | EVar x         -> showName x

let rec lam cotele exp =
  match cotele with
  | []           -> exp
  | (x, t) :: xs -> ELam (x, t, lam xs exp)

let rec app f = function
  | []      -> f
  | x :: xs -> app (EApp (f, x)) xs