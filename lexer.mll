{

  open Parser
  exception Lexing_error of string

}
(*   (:requirements :strips :derived-predicates) *)
let space = [' ' '\t' '\r' '\n']
rule token = parse
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "define" {DEFINE}
  | ":requirements" {REQUIREMENTS}
  | ":derived-predicates" {DPREDICATES}
  | ":strips" {STRIPS}
  | ['a' - 'z' 'A' - 'z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as id { NAME id }
   
  (*
  | "?" {QUESTIONMARK} 
  | "-" {DASH}
  | "symbol" {SYMBOL}
  | "predicates" {PREDICATES}
  | "action" {ACTION}
  | "parameters" {PARAMETERS}
  | "precondition" {PRECONDITION}
  | "effect"  {EFFECT}
  | "not" {NOT} *)
  | space+ { token lexbuf }
  | eof {EOF}