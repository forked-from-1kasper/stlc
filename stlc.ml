module SM = Map.Make (String)
type vars = int SM.t

type name = string

type texp =
| TArr of texp * texp
| TNat | TBool

type exp =
| ELam of name * texp * exp
| EApp of exp * exp
| EVar of name
| EZero | ESucc
| ETrue | EFalse | EIte

let replicate size value =
  List.init size (fun _ -> value)

let const idx value =
  replicate idx "⊢-rec" @ [value]

let value : int ref = ref 0
let gensym () =
  value := !value + 1;
  Printf.sprintf "?%d" !value

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

let rec eval = function
  | EApp (f, x)    ->
    begin match eval f with
    | ELam (z, _, p)             -> subst z (eval x) (eval p)
    | EApp (EApp (EIte, b), y) ->
      begin match b with
      | ETrue  -> y
      | EFalse -> eval x
      | _      -> failwith "“ite” expects a boolean"
      end
    | g                          -> EApp (g, eval x)
    end
  | ELam (x, t, p) -> ELam (x, t, eval p)
  | tau            -> tau

let rec infer vars idx = function
  | EZero          -> const idx "0-def"
  | ESucc          -> const idx "succ-def"
  | EFalse         -> const idx "false-def"
  | ETrue          -> const idx "true-def"
  | EIte           -> const idx "ite-def"
  | EVar x         ->
    begin match SM.find_opt x vars with
    | Some depth -> "ctx-intro" :: replicate (idx - depth) "ctx-rec" @ ["ctx-∈"]
    | None       -> failwith (Printf.sprintf "unbound variable “%s”" x)
    end
  | EApp (f, x)    -> "λ-elim" :: (infer vars idx f @ infer vars idx x)
  | ELam (x, t, y) -> "λ-intro" :: infer (SM.add x (idx + 1) vars) (idx + 1) y

let rec showTExp : texp -> string = function
  | TArr (dom, cod) -> Printf.sprintf "(%s → %s)" (showTExp dom) (showTExp cod)
  | TNat            -> "ℕ"
  | TBool           -> "bool"

let rec showExp : exp -> string = function
  | EZero          -> "0"
  | ESucc          -> "succ"
  | EFalse         -> "false"
  | ETrue          -> "true"
  | EIte           -> "ite"
  | EVar x         -> x
  | EApp (f, x)    -> Printf.sprintf "(%s %s)" (showExp f) (showExp x)
  | ELam (x, t, y) -> Printf.sprintf "(λ (%s : %s) %s)" x (showTExp t) (showExp y)

let check name e t : string =
  Printf.sprintf "(theorem - %s (· ⊢ %s : %s) %s)"
    name (showExp e) (showTExp t)
    (infer SM.empty 0 e |> String.concat " ")

let term1 = EApp (EApp (EApp (EIte, ETrue), EApp (ESucc, EZero)), EZero)
let term2 = ELam ("x", TNat, ELam ("y", TNat, EApp (ESucc, EApp (ESucc, EVar "x"))))
let term3 = EApp (EApp (EApp (EIte, EFalse), EApp (ESucc, EZero)), EZero)

let () =
  term3
  |> showExp
  |> Printf.printf "%s\n";

  conv term3
  |> eval
  |> showExp
  |> Printf.printf "%s\n";

  check "term1" term1 TNat
  |> Printf.printf "%s\n";

  check "term2" term2 (TArr (TNat, TArr (TNat, TNat)))
  |> Printf.printf "%s\n";