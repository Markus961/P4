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

  (*Parse*)
  let input_channel = open_in "./data/domain.pddl" in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in input_channel;

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

(* lexer tests *)
open Parser

let format_token t =
  match t with
  | DOMAIN -> "DOMAIN"
  | PROBLEMDOMAIN -> "PROBLEMDOMAIN"
  | STRIPS -> "STRIPS"
  | EOF -> "EOF"
  | VAR v -> "VAR \"" ^ v ^ "\""
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | DEFINE -> "DEFINE"
  | PROBLEM -> "PROBLEM"
  | NAME n -> "NAME \"" ^ n ^ "\""
  | OBJECTS -> "OBJECTS"
  | INIT -> "INIT"
  | GOAL -> "GOAL"
  | _ -> "other"

let print_tokens tokens =
  print_string "[ ";
  List.iter (fun t -> print_string ((format_token t) ^ "; ")) tokens;
  print_endline "]"


let tokens_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    match Lexer.token lexbuf with
    | EOF -> List.rev (acc)
    | t -> loop (t :: acc)
  in
  loop []

let lexer_of_tokens (toks: Parser.token list) : Lexing.lexbuf -> Parser.token =
  let r = ref toks in
  fun _lexbuf ->
    match !r with
    | t :: rest -> r := rest; t
    | [] -> EOF
  
  
let test_lexer test_name text expected =
  let tokens = tokens_of_string text in
    print_tokens tokens;
    print_tokens expected;
    if not (tokens = expected) then raise (Failure ("test  "^ test_name ^ " failed :-("))
    else print_endline ("test " ^ test_name ^ " passed :-)")

let test_parser test_name tokens expected =
  let dummy = Lexing.from_string "" in
  let ast = Parser.prog (lexer_of_tokens (tokens @ [EOF])) dummy in
    print_tokens tokens;
    (*print_ast expected;*)
    if not (ast = {defs = expected}) then raise (Failure ("test  "^ test_name ^ " failed :-("))
    else print_endline ("test " ^ test_name ^ " passed :-)")

let () =
  print_endline "**MAYA TEST";
  test_lexer "simple test" "domain :strips" [DOMAIN; STRIPS]; 
  test_lexer "test2" "domain :init" [DOMAIN; INIT]; 
  test_lexer "test var" "?abc" [VAR "?abc"]; 
  (* test_lexer "xxr" "(define (problem testname) (:domain problemname) (:objects xx) (:init) (:goal (xx)))" [EOF]; *)
  let tokens = [ LPAREN; DEFINE; LPAREN; PROBLEM; NAME "testname"; RPAREN; LPAREN; PROBLEMDOMAIN; NAME "problemname"; RPAREN; LPAREN; OBJECTS; NAME "obj"; RPAREN; LPAREN; INIT; RPAREN; LPAREN; GOAL; LPAREN; NAME "xx"; RPAREN; RPAREN; RPAREN ] in
  let expected = ProblemDef { problem = {problem_name = "testname"}; problemdomain = {problemdomain_name = "problemname"}; objects = NormalObjects ["obj"]; init = []; goal = Atom ("xx", [])} in
  test_parser "minimal parser test" tokens expected;




