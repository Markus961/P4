(* Read one input file, transform it, and write the matching output file. *)
let process_file input_path =
  print_endline "START"; (* Debug: did the function start *)
  let input_channel = open_in input_path in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in input_channel;

  (* Only problem files need extra validation before transformation. *)
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

  (* Convert the transformed AST to PDDL text and write it to disk. *)
  match transformed_ast.defs with
  | DomainDef d ->
    print_endline "DEBUG DOMAIN REACHED";

    print_endline "Generating DOMAIN file...";

    let content = Domain_generator.string_of_domain d in
    Utils.write_file "transformed_domain.pddl" content;

    print_endline "Wrote transformed_domain.pddl"

  | ProblemDef p ->
    print_endline "Generating PROBLEM file...";

    let content = Problem_generator.string_of_problem p in
    Utils.write_file "transformed_problem.pddl" content;

    print_endline "Wrote transformed_problem.pddl"
