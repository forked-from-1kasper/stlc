open Prelude
open Lexing
open Check
open Expr
open Eval

open Parser

let help =
  "    invoke = stlc | stlc list
       list = [] | command list
    command = check filename
            | eval  filename
            | help"

let (>>) f g = fun x -> g (f x)

exception Parser of int * int * string

let parse filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try Parser.file Lexer.main lexbuf
  with Parser.Error ->
    Parser (lexeme_start lexbuf, lexeme_end lexbuf, lexeme lexbuf)
    |> raise

type cmdline =
  | Check of string
  | Eval  of string
  | Help

let environment : env ref = ref SM.empty
let check : command -> unit = function
  | Decl (name, texp, exp) -> begin
    Printf.printf "%s\n" (emit (Some name) exp texp);
    environment := SM.add name exp !environment
  end
  | Eval (texp, exp) ->
    print_endline (emit None exp texp)
  | Abbrev (name, params, texp) ->
    let patt = TVar (name, params) in
    Printf.printf "(define %s %s)\n" (showTExp patt) (showTExp texp)

let evaluate : command -> unit = function
  | Decl (name, texp, exp) ->
    environment := SM.add name exp !environment
  | Eval (_, exp) ->
    eval !environment exp
    |> showExp
    |> Printf.printf "%s ⇒ %s\n" (showExp exp)
  | _ -> ()

let file fn = parse >> List.iter fn
let cmd : cmdline -> unit = function
  | Check filename -> file check    filename
  | Eval  filename -> file evaluate filename
  | Help           -> print_endline help

let rec parseArgs : string list -> cmdline list = function
  | [] -> []
  | "check" :: filename :: rest -> Check filename :: parseArgs rest
  | "eval"  :: filename :: rest -> Eval  filename :: parseArgs rest
  | "help"  :: rest -> Help :: parseArgs rest
  | x :: xs ->
    Printf.printf "Unknown command “%s”\n" x;
    parseArgs xs

let defaults : cmdline list -> cmdline list = function
  | [] -> [Help]
  | xs -> xs

let () =
  Array.to_list Sys.argv
  |> List.tl |> parseArgs |> defaults
  |> List.iter cmd