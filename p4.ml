open Ast

(* Converts features from the parser into strings *)
let string_of_feature = function
  | Strips -> ":strips"
  | DerivedPredicates -> ":derived-predicates"

let () =
  (* opens a little snippet of the domain.pddl *)
  let filename = "domain-mini.pddl" in
  (* opens the file and creates a lexer buffer. *)
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  (* it parses using the parser entrypoint and lexer token . *)
  match Parser.prog Lexer.token lexbuf with
  | result ->
    Printf.printf "Parsed domain: %s\n" result.defs.domain_name;
    Printf.printf "Requirements: %s\n" (String.concat ", " (List.map string_of_feature result.main.features));
    close_in input_channel
  | exception Failure msg ->
    Printf.printf "Parse error: %s\n" msg;
    close_in input_channel
