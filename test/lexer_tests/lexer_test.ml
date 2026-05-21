(* lexer tests *)
open Parser
open Ast

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
  
  
let test_lexer text expected =
  let tokens = tokens_of_string text in
    (*print_tokens tokens;*)
    (*print_tokens expected;*)
    if tokens <> expected then begin
      print_endline("FAILED");
      print_endline("Actual:");
      print_tokens tokens;
      print_endline("Expected:");
      print_tokens expected      
    end else
      print_endline ("PASSED")

let test_parser tokens expected =
  let dummy = Lexing.from_string "" in
  let ast = Parser.prog (lexer_of_tokens (tokens @ [EOF])) dummy in
    (*print_tokens tokens;*)
    (*print_ast expected;*)
    if ast <> {defs = expected} then begin
      print_endline ("FAILED");
      print_endline("Actual:");
      print_tokens tokens
    end else
      print_endline ("PASSED")

let () =
  print_endline("Running lexer test 1");
  test_lexer "domain :strips" [DOMAIN; STRIPS]; 
  print_endline("Running lexer test 2");
  test_lexer "domain :init" [DOMAIN; INIT]; 
  print_endline("Running lexer test var");
  test_lexer "?abc" [VAR "?abc"]; 
  (* test_lexer "xxr" "(define (problem testname) (:domain problemname) (:objects xx) (:init) (:goal (xx)))" [EOF]; *)
  let tokens = [ LPAREN; DEFINE; LPAREN; PROBLEM; NAME "testname"; RPAREN; LPAREN; PROBLEMDOMAIN; NAME "problemname"; RPAREN; LPAREN; OBJECTS; NAME "obj"; RPAREN; LPAREN; INIT; RPAREN; LPAREN; GOAL; LPAREN; NAME "xx"; RPAREN; RPAREN; RPAREN ] in
  let expected = ProblemDef { problem = {problem_name = "testname"}; problemdomain = {problemdomain_name = "problemname"}; objects = NormalObjects ["obj"]; gl = []; init = []; goal = Atom ("xx", [])} in
  print_endline("Running minimal parser test");
  test_parser tokens expected;