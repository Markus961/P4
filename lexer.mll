{

  open Parser
  exception Lexing_error of string

}

let space = [' ' '\t' '\r' '\n']
rule token = parse
  | "define" {DEFINE}
  | "domain" {DOMAIN}
  | ":requirements" {REQUIREMENTS}
  | ":strips" {STRIPS}
  | ":derived-predicates" {DPREDICATES}
  | ":predicates" {PREDICATES}
  | ":derived" {DERIVED}
  | ":action" {ACTION}
  | ":parameters" {PARAMETERS}
  | ":precondition" {PRECONDITION}
  | ":effect"  {EFFECT}
  | "and" {AND}
  | "exists" {EXISTS}
  | "not" {NOT} 
  | "(" {LPAREN}
  | ")" {RPAREN}  
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { NAME id }  
  | '?' ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { VAR id } (* Because all variables start with '?' *)
  | ";"  [^ '\n']* {token lexbuf} (*Comment handling in PDDL*)
  | space+ { token lexbuf }
  | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) } (* Golden *)
  | eof {EOF}