(* husk at lave executable
open Ast

(* function to parse input-file into ast *)

(* function to transform ast into standard pddl ast *)

(* function to do something with transformed ast (make pddl) *)
*)

open Ast

(* helper function - Writes a string to a file *)
let write_file filename content =
  (* Open a file for writing and get an output channel *)
  let oc = open_out filename in

  (* Write the entire string content into the file *)
  output_string oc content;

  (* Close the file to ensure all data is flushed and saved *)
  close_out oc

let () =
  print_endline "START"; (* Debug: did the function start *)
  let input_channel = open_in "./data/arrayconstructs.pddl" in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in input_channel;

  (match ast.defs with
  | DomainDef _ -> ()
  | ProblemDef problem_def ->
      Ast_validator.validate_problem_def problem_def); (*Call the Ast_validator to check the input*)

  (*Transform*)
  let transformed_ast = Ast_transformer.transform_program ast in

  (* Debug print - code for writing to terminal (optional) *)
  let debug = true in
  if debug then (
    print_endline "=== DEBUG OUTPUT ===";
    Pddl_printer.print_program transformed_ast
  );

  (*Generate string and write to file*)
  match transformed_ast.defs with
  | DomainDef d ->
    print_endline "DEBUG DOMAIN REACHED";

    print_endline "Generating DOMAIN file...";

    let content = Domain_generator.string_of_domain d in
    write_file "transformed_domain.pddl" content;

    print_endline "Wrote transformed_domain.pddl"

  | ProblemDef p ->
    print_endline "Generating PROBLEM file...";

    let content = Problem_generator.string_of_problem p in
    write_file "transformed_problem.pddl" content;

    print_endline "Wrote transformed_problem.pddl"


  (* before code used to print to terminal *)
  (* 
  match transformed_ast.defs with
    | DomainDef _ ->
      print_endline "DOMAIN"; (* Remove from final program *)
      Pddl_printer.print_program transformed_ast
    | ProblemDef _ ->
      print_endline "PROBLEM";
      Pddl_printer.print_program transformed_ast
  *)



