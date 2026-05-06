open Ast

let run input_file =
  let input_channel = open_in input_file in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in input_channel;

  (match ast.defs with
   | DomainDef _ -> ()
   | ProblemDef p -> Ast_validator.validate_problem_def p);

  let transformed_ast = Ast_transformer.transform_program ast in

  match transformed_ast.defs with
  | DomainDef d ->
      Domain_generator.string_of_domain d

  | ProblemDef p ->
      Problem_generator.string_of_problem p