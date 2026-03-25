(*
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
      
(*let string_of_derived = function
  | { pdef; logic = [] } ->
    Printf.sprintf "(%s)" pdef
  | { pdef; logic } ->
      Printf.sprintf "(%s %s)" pdef (String.concat " " logic)*)

let () =
  (* opens a little snippet of the domain.pddl *)
  let filename = "problem.pddl" in
  (* opens the file and creates a lexer buffer. *)
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  (* it parses using the parser entrypoint and lexer token . *)
  match Parser.prog Lexer.token lexbuf with
  | result ->
    (*Printf.printf "Parsed domain: %s\n" result.defs.domain.domain_name;
    Printf.printf "Requirements: %s\n" (String.concat " " (List.map string_of_feature result.defs.requirements.features));
    Printf.printf "Predicates: %s\n" (String.concat "\n " (List.map string_of_predicate result.defs.predicates));
    Printf.printf "Derived: %s\n"  (String.concat "\n " (List.map string_of_predicate result.defs.derived));*)

    close_in input_channel
  | exception Failure msg ->
    Printf.printf "Parse error: %s\n" msg;
    close_in input_channel


*)

(* p4.ml *)

open Ast  (* antager ast.ml har alle typer og funktioner *)

let () =
  (* forestil dig at parseren har lavet dette AST *)
  let my_objects_decl = GridAndObjects (6, 6, ["triangle"; "diamond"; "key0"]) in

  (* Oversæt til odef list *)
  let all_odefs = objects_decl_to_odef my_objects_decl in

  (* Print til konsol *)
  print_endline "Expanded objects from grid:";
  List.iter (fun o -> print_endline o.oname) all_odefs;

  (* Brug all_odefs til at lave ProblemDef *)
  let _problem_ast = ProblemDef {
  problem = {problem_name="test_problem"};
  problemdomain = {problemdomain_name="test_domain"};
  objects = my_objects_decl;
  init = [];
  goal = Atom("dummy", []);
  } in
  ()