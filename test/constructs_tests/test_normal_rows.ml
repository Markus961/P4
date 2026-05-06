open Ast

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let () =
  let input_channel = open_in "normal_rows.pddl" in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in input_channel;
  
   (match ast.defs with
  | DomainDef _ -> ()
  | ProblemDef problem_def ->
      Ast_validator.validate_problem_def problem_def); (*Call the Ast_validator to check the input*)

  (*Transform*)
  let transformed_ast = Ast_transformer.transform_program ast in

  (*Generate string and write to file*)
  match transformed_ast.defs with
  | DomainDef d ->

    let content = Domain_generator.string_of_domain d in
    write_file "transformed_domain.pddl" content;

  | ProblemDef p ->
    print_endline "Generating PROBLEM file...";

    let content = Problem_generator.string_of_problem p in
    write_file "transformed.pddl" content;


let read_file filename =
  let ic = open_in filename in
  let length = in_channel_length ic in
  let content = really_input_string ic length in
  close_in ic;
  content in

let expected = read_file "expected.pddl" in
let actual = read_file "transformed.pddl" in

assert (expected = actual)



