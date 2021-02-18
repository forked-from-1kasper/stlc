open Prelude
open Lexing
open Check
open Expr
open Eval

open Parser

let (>>) f g = fun x -> g (f x)

exception Parser of int * int * string

let parse filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try Parser.file Lexer.main lexbuf
  with Parser.Error ->
    Parser (lexeme_start lexbuf, lexeme_end lexbuf, lexeme lexbuf)
    |> raise

let environment : env ref = ref SM.empty
let cmd : command -> unit = function
  | Decl (name, texp, exp) -> begin
    Printf.printf "%s\n" (emit name exp texp);
    environment := SM.add name exp !environment
  end

let file = List.iter cmd

let () =
  Array.to_list Sys.argv
  |> List.tl |> List.iter (parse >> file)