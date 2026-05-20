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
  print_endline "**TEST";
  test_lexer "simple test" "domain :strips" [DOMAIN; STRIPS]; 
  test_lexer "test2" "domain :init" [DOMAIN; INIT]; 
  test_lexer "test var" "?abc" [VAR "?abc"]; 
  (* test_lexer "xxr" "(define (problem testname) (:domain problemname) (:objects xx) (:init) (:goal (xx)))" [EOF]; *)
  let tokens = [ LPAREN; DEFINE; LPAREN; PROBLEM; NAME "testname"; RPAREN; LPAREN; PROBLEMDOMAIN; NAME "problemname"; RPAREN; LPAREN; OBJECTS; NAME "obj"; RPAREN; LPAREN; INIT; RPAREN; LPAREN; GOAL; LPAREN; NAME "xx"; RPAREN; RPAREN; RPAREN ] in
  let expected = ProblemDef { problem = {problem_name = "testname"}; problemdomain = {problemdomain_name = "problemname"}; objects = NormalObjects ["obj"]; grid = None; init = []; goal = Atom ("xx", [])} in
  test_parser "minimal parser test" tokens expected;