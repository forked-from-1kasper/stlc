module SM = Map.Make (String)
type vars = int SM.t

type name = string

type texp =
| TArr of texp * texp
| TVar of string

type exp =
| ELam   of name * texp * exp
| EApp   of exp * exp
| EVar   of name

let replicate size value =
  List.init size (fun _ -> value)

let const idx value =
  replicate idx "⊢-rec" @ [value]

let def decl = decl ^ "-def"

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
    | ELam (z, _, p) -> subst z (eval x) (eval p)
    | g -> EApp (g, eval x)
    end
  | ELam (x, t, p) -> ELam (x, t, eval p)
  | tau            -> tau

let rec infer vars idx = function
  | EVar x         ->
    begin match SM.find_opt x vars with
    | Some depth -> "ctx-intro" :: replicate (idx - depth) "ctx-rec" @ ["ctx-∈"]
    | None       -> const idx (def x)
    end
  | EApp (f, x)    -> "λ-elim" :: (infer vars idx f @ infer vars idx x)
  | ELam (x, t, y) -> "λ-intro" :: infer (SM.add x (idx + 1) vars) (idx + 1) y

let rec showTExp : texp -> string = function
  | TArr (dom, cod) -> Printf.sprintf "(%s → %s)" (showTExp dom) (showTExp cod)
  | TVar value    -> value

let rec showExp : exp -> string = function
  | EVar x         -> x
  | EApp (f, x)    -> Printf.sprintf "(%s %s)" (showExp f) (showExp x)
  | ELam (x, t, y) -> Printf.sprintf "(λ (%s : %s) %s)" x (showTExp t) (showExp y)

let check name e t =
  Printf.sprintf "(theorem - %s-check (· ⊢ %s : %s) %s)"
    name (showExp e) (showTExp t)
    (infer SM.empty 0 e |> String.concat " ")

let decl name t =
  Printf.sprintf "(postulate - %s (· ⊢ %s : %s))"
    (def name) name (showTExp t)

let emit name e t =
  check name e t ^ "\n" ^ decl name t

let ite = EVar "ite"

let tt = EVar "true"
let ff = EVar "false"

let zero = EVar "0"
let succ = EVar "succ"

let boolean = TVar "bool"
let nat     = TVar "ℕ"

let term1 = EApp (EApp (EApp (ite, tt), EApp (succ, zero)), zero)
let term2 = ELam ("x", nat, ELam ("y", nat, EApp (succ, EApp (succ, EVar "x"))))
let term3 = EApp (EApp (EApp (ite, ff), EApp (succ, zero)), zero)

let () =
  term3
  |> showExp
  |> Printf.printf "%s\n";

  conv term3
  |> eval
  |> showExp
  |> Printf.printf "%s\n";

  emit "term1" term1 nat
  |> Printf.printf "%s\n";

  emit "term2" term2 (TArr (nat, TArr (nat, nat)))
  |> Printf.printf "%s\n";

  emit "term3" term3 nat
  |> Printf.printf "%s\n"