{

  open Parser
  exception Lexing_error of string

}

let space = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let integer = digit+

rule token = parse
  | "define" {DEFINE}
  (*Domain File*)
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
  (*Problem File*)
  | "problem" {PROBLEM}
  | ":domain" {PROBLEMDOMAIN}
  | ":objects" {OBJECTS}
  | ":init" {INIT}
  | ":goal" {GOAL}
  | ":grid" {GRID}
  | "locked_nodes" {LOCKEDNODES}
  (*Logic*)
  | "not" {NOT} 
  | "and" {AND}
  | "exists" {EXISTS}
  | "(" {LPAREN}
  | ")" {RPAREN}  
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | integer as c { CONST (int_of_string c) }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { NAME id }  
  | ['a'-'z' 'A'-'Z' '0'-'9'] as id {CHARACTER id}
  | '?' ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { VAR id } (* Because all variables start with '?' *)
  | ";"  [^ '\n']* {token lexbuf} (*Comment handling in PDDL*)
  | space+ { token lexbuf }
  | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) } (* Golden *)
  | eof {EOF}