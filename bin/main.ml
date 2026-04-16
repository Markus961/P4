(* husk at lave executable
open Ast

(* function to parse input-file into ast *)

(* function to transform ast into standard pddl ast *)

(* function to do something with transformed ast (make pddl) *)
*)

open Ast

let () =
  print_endline "START"; (* Debug: did the function start *)
  let input_channel = open_in "./data/domain.pddl" in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in input_channel;

  let transformed_ast = Ast_transformer.transform_program ast in
 
  match transformed_ast.defs with
    | DomainDef _ ->
      print_endline "DOMAIN"; (* Remove from final program *)
      Pddl_printer.print_program transformed_ast
    | ProblemDef _ ->
      print_endline "PROBLEM";
      Pddl_printer.print_program transformed_ast

