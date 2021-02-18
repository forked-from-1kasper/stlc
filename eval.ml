open Prelude
open Expr

let conv =
  let rec func map = function
    | EVar x when SM.mem x map -> EVar (SM.find x map)
    | EApp (f, x)              -> EApp (func map f, func map x)
    | ELam (x, t, p)           ->
      let y = gensym () in
      ELam (y, t, func (SM.add x y map) p)
    | tau                      -> tau in
  func SM.empty

let rec subst x v = function
  | EVar y when x = y -> v
  | EApp (f, y)       -> EApp (subst x v f, subst x v y)
  | ELam (y, t, u)    -> ELam (y, t, subst x v u)
  | tau               -> tau

let rec eval env = function
  | EApp (f, x)    ->
    let y = eval env x in
    begin match eval env f with
    | ELam (z, _, p) -> subst z y (eval env p)
    | g              -> EApp (g, y)
    end
  | ELam (x, t, p) -> ELam (x, t, eval env p)
  | EVar x         -> Option.value (SM.find_opt x env) ~default:(EVar x)