open Lexer (* ændres til det vores lexer hedder *)

let rec print_tokens lexbuf = 
  (* kald til "rule token = parse". lexbuf er objektet som lexeren læser fra *)
  match token lexbuf with
  | LPAREN -> Printf.printf "LPAREN\n"; print_tokens lexbuf
  | RPAREN -> Printf.printf "RPAREN\n"; print_tokens lexbuf
  | NAME s -> Printf.printf "NAME %s\n" s; print_tokens lexbuf
  | CONST i -> Printf.printf "CONST %d\n" i; print_tokens lexbuf
  | EOF -> Printf.printf "EOF\n"

let () = 
  let input = "(define (domain grid))" in
  (* Lexing.from_string gør en string klar til lexeren *)
  let lexbuf = Lexing.from_string input in
  print_tokens lexbuf
