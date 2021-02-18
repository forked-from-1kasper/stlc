open Prelude
open Check
open Expr
open Eval

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
  |> eval SM.empty
  |> showExp
  |> Printf.printf "%s\n";

  emit "term1" term1 nat
  |> Printf.printf "%s\n";

  emit "term2" term2 (TArr (nat, TArr (nat, nat)))
  |> Printf.printf "%s\n";

  emit "term3" term3 nat
  |> Printf.printf "%s\n"