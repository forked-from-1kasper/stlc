open Prelude
open Expr

let rec infer vars idx = function
  | EVar (Ident x) ->
    begin match SM.find_opt x vars with
    | Some depth -> "ctx-intro" :: replicate (idx - depth) "ctx-rec" @ ["ctx-∈"]
    | None       -> const idx (def x)
    end
  | EApp (f, x) -> "λ-elim" :: (infer vars idx f @ infer vars idx x)
  | ELam (Ident x, t, y) -> "λ-intro" :: infer (SM.add x (idx + 1) vars) (idx + 1) y
  | _ -> failwith "attempt to check erased term"

let check name e t =
  Printf.sprintf "(theorem - %s-check (· ⊢ %s : %s) %s)"
    name (showExp e) (showTExp t)
    (infer SM.empty 0 e |> String.concat " ")

let decl name t =
  Printf.sprintf "(postulate - %s (· ⊢ %s : %s))"
    (def name) name (showTExp t)

let emit name e t =
  check name e t ^ "\n" ^ decl name t  