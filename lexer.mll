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
  | "domain" {DOMAIN}
  | ":requirements" {REQUIREMENTS}
  | ":derived-predicates" {DPREDICATES}
  | ":strips" {STRIPS}
  | ['a' - 'z' 'A' - 'z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_' '-']* as id { NAME id }  
  | '?' ['a' - 'z' 'A' - 'z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_' '-']* as id { VAR id } (* Because all variables start with '?' *)
  (* 
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