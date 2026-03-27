(*let test input = 
  let lexbuf = Lexing.from_string input in

  let rec loop () = 
    let tok = Lexer.token lexbuf in
    print_endline (Parser.show_token tok);
    if tok <> Parser.EOF then loop ()
  in
  loop ()*)