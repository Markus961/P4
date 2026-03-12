{

  open Parser
  exception Lexing_error of string

}


let space = [' ' '\t']
rule token = parse

  | "(" {LPAREN}
  | ")" {RPAREN}
  | "-" {DASH}
  | "?" {QUESTIONMARK}
  | space+ { token lexbuf }
  | ":action" {ACTION}
  | ":parameters" {PARAMETERS}
  | ":precondition" {PRECONDITION}
  | ":effect"  {EFFECT}
  | ":not" {NOT}
  | ['a' - 'z' 'A' - 'z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as id { NAME id }