open P4
open Lexer (* ændres til det vores lexer hedder *)

exception Lexing_error of string
let rec print_tokens lexbuf = 
  (* calls "rule token = parse". lexbuf is the object from which the lexer reads *)
  match token lexbuf with
  | LPAREN -> Printf.printf "LPAREN\n"; print_tokens lexbuf
  | RPAREN -> Printf.printf "RPAREN\n"; print_tokens lexbuf
  | DEFINE -> Printf.printf "DEFINE\n"; print_tokens lexbuf
  | DOMAIN -> Printf.printf "DOMAIN\n"; print_tokens lexbuf
  | REQUIREMENTS -> Printf.printf "REQUIREMENTS\n"; print_tokens lexbuf
  | DPREDICATES -> Printf.printf "DPREDICATES\n"; print_tokens lexbuf
  | PREDICATES -> Printf.printf "PREDICATES\n"; print_tokens lexbuf
  | STRIPS -> Printf.printf "STRIPS\n"; print_tokens lexbuf
  | NAME id -> Printf.printf "NAME %s\n" id; print_tokens lexbuf
  | VAR id -> Printf.printf "VAR %s\n" id; print_tokens lexbuf
  | c -> raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c))
  | EOF -> Printf.printf "EOF\n"

let () =
  let filename = "domain-mini.pddl" in
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  print_tokens lexbuf