{
  open Parser
  open Lexing
}

let bytes1  = [^ '\t' ' ' '\r' '\n' '(' ')' ':' '.' ',' '/']
let bytes2  = ['\192'-'\223']['\128'-'\191']
let bytes3  = ['\224'-'\239']['\128'-'\191']['\128'-'\191']
let bytes4  = ['\240'-'\247']['\128'-'\191']['\128'-'\191']['\128'-'\191']
let utf8    = bytes1|bytes2|bytes3|bytes4
let ws      = ['\t' ' ' '\r' '\n']
let nl      = ['\r' '\n']
let comment = "--" [^ '\n' '\r']* (nl|eof)
let colon   = ':'
(* arrow := -> | →
   defeq := := | ≔ | ≜ | ≝
   lam   := \  | λ *)
let arrow   = "->"|"\xE2\x86\x92"
let defeq   = ":="|"\xE2\x89\x94"|"\xE2\x89\x9C"|"\xE2\x89\x9D"
let lam     = "\\"|"\xCE\xBB"

rule main = parse
| comment         { main lexbuf }
| ws+             { main lexbuf }
| ','             { COMMA       }
| '('             { LPARENS     }
| ')'             { RPARENS     }
| "#eval"         { EVAL        }
| "def"           { DEF         }
| "abbrev"        { ABBREV      }
| defeq           { DEFEQ       }
| lam             { LAM         }
| arrow           { ARROW       }
| colon           { COLON       }
| utf8+ as s      { IDENT s     }
| eof             { EOF         }