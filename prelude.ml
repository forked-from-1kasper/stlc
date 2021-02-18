let replicate size value =
  List.init size (fun _ -> value)

let const idx value =
  replicate idx "⊢-rec" @ [value]

let def decl = decl ^ "-def"

let value : int ref = ref 0
let gensym () =
  value := !value + 1;
  Printf.sprintf "?%d" !value