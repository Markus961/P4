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
  | ":predicates" {PREDICATES}
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
  (*Grid Part Problem File*)
  | "("  { lparen_or_grid lexbuf }
  | ":rows" {ROWS}
  | ":columns" {COLUMNS}
  | ":name" {GRIDNAME}
  | ":connections" {CONNECTIONS}
  | ":keys" {KEYS}
  | ":shapes" {SHAPES}
  | ":lockedlocations" {LOCKEDNODESMATRIX}
  | ":lockednodesarray" {LOCKEDNODES}
  | ":keylocations" {KEYLOCATIONMATRIX}
  (*Logic*)
  | "not" {NOT} 
  | "and" {AND}
  | "exists" {EXISTS}
  (* LPAREN is defined further in lparen_or_grid below to fix new ambiguity related to GRID token ambiguity fix *)
  | ")" {RPAREN}  
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "," {COMMA}
  | "=" {EQUALS}
  | "+" {PLUS}
  | "*" {MULT}
  | integer as c { CONST (int_of_string c) }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { NAME id }  
  | '?' ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { VAR id } (* Because all variables start with '?' *)
  | '-' ['a'-'z' 'A'-'Z'] as id {FLAG id}
  | ";"  [^ '\n']* {token lexbuf} (*Comment handling in PDDL*)
  | space+ { token lexbuf }
  | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) } (* Golden *)
  | eof {EOF}

and lparen_or_grid = parse
  | space* ":grid" { LPAREN_GRID }
  | ""              { LPAREN }