open Ast
let run input_file =
  let input_channel = open_in input_file in
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
    let content = Domain_generator.string_of_domain d in
    Utils.write_file "transformed_domain.pddl" content;

  | ProblemDef p ->
    let content = Problem_generator.string_of_problem p in
    Utils.write_file "transformed_problem.pddl" content