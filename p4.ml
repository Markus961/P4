open Ast

(* Converts features from the parser into strings *)
let string_of_feature = function
  | Strips -> ":strips"
  | DerivedPredicates -> ":derived-predicates"

(* Converts predicates frorm the parser into strings *)
let string_of_predicate = function
  | { pname; variables = [] } ->
      Printf.sprintf "(%s)" pname
  | { pname; variables } ->
      Printf.sprintf "(%s %s)" pname (String.concat " " variables)
      
let string_of_derived = function
  | { pdef; logic = [] } ->
    Printf.sprintf "(%s)" pdef
  | { pdef; logic } ->
      Printf.sprintf "(%s %s)" pdef (String.concat " " logic)

let () =
  (* opens a little snippet of the domain.pddl *)
  let filename = "domain-mini.pddl" in
  (* opens the file and creates a lexer buffer. *)
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  (* it parses using the parser entrypoint and lexer token . *)
  match Parser.prog Lexer.token lexbuf with
  | result ->
    Printf.printf "Parsed domain: %s\n" result.defs.domain.domain_name;
    Printf.printf "Requirements: %s\n" (String.concat " " (List.map string_of_feature result.defs.requirements.features));
    Printf.printf "Predicates: %s\n" (String.concat "\n " (List.map string_of_predicate result.defs.predicates));
    Printf.printf "Derived: %s\n"  (String.concat "\n " (List.map string_of_predicate result.defs.derived));

    close_in input_channel
  | exception Failure msg ->
    Printf.printf "Parse error: %s\n" msg;
    close_in input_channel
