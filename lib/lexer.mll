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
  | "locked_nodes_matrix" {LOCKEDNOTESMATRIX}
  | "locked_nodes" {LOCKEDNODES}
  | "grid_connection" {GRIDCONNECTION}
  | "open_nodes" {OPENNODES}
  | ":keys" {KEYS}
  | "keylocation_matrix" {KEYLOCATIONMATRIX}
  (*Logic*)
  | "not" {NOT} 
  | "and" {AND}
  | "exists" {EXISTS}
  | "(" {LPAREN}
  | ")" {RPAREN}  
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "," {COMMA}
  | "+" {PLUS}
  | "*" {MULT}
  | ['a'-'z' 'A'-'Z' '0'-'9' '-'] as c { CHARACTER c }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { NAME id }    
  | integer as c { CONST (int_of_string c) }
  | '?' ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { VAR id } (* Because all variables start with '?' *)
  | '-' ['a'-'z' 'A'-'Z'] as id {FLAG id}
  | ";"  [^ '\n']* {token lexbuf} (*Comment handling in PDDL*)
  | space+ { token lexbuf }
  | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) } (* Golden *)
  | eof {EOF}