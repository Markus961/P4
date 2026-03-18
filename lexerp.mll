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
  | "problem" {PROBLEM}
  | ":domain" {DOMAIN}
  | ":objects" {OBJECTS}
  | ":init" {INIT}
  | ":goal" {GOAL}
  | ['a'-'z' 'A'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { NAME id }  
  | '?' ['a'-'z' 'A'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as id { VAR id } (* Because all variables start with '?' *)
  | space+ { token lexbuf }
  | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) } (* Golden *)


  | eof {EOF}