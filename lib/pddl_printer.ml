open Ast
(* open Ast_transformer *)

let print_objects_decl obj =
  match obj with
  | NormalObjects objs ->
      List.iter print_endline objs
  | _ -> 
    print_endline ("No objects")

(* Used for printing predicate definitions from domain.pddl *)
let print_pdefinition pdefs =
  print_endline ("(" ^ pdefs.pname);
  List.iter print_endline pdefs.variables;
  print_endline (")")

let print_all_pdefinitions pdefinitions =
  List.iter print_pdefinition pdefinitions

(* These funcitons are used for printing derived predicates from domain.pddl *)
(* string_of_expr is a function that takes the ast expr and translates it to a string *)  
let rec string_of_expr e =
  match e with
  | Atom (name, args) ->
       "(" ^ name ^ " " ^ String.concat " " args ^ ")"
  | And exprs ->
      "(and " ^ String.concat " " (List.map string_of_expr exprs) ^ ")"
  | Not e ->
      "(not " ^ string_of_expr e ^ ")"
  | Exists (vars, e) ->
      "(exists (" ^ String.concat " " vars ^ ") " ^ string_of_expr e ^ ")"  
      
(* This function is a pretteprinter for the derived predicates from the domain.pddl. It takes one derived value and print it in pddl *)      
let print_derived d =
  print_endline ("(:derived (" ^ d.header.pname ^ " " ^ String.concat " " d.header.variables ^ ") " ^ string_of_expr d.body ^ ")"
  )

(* This function iterates the print_derived from earlier and creates a list *)
let print_all_derived dlist =
  List.iter print_derived dlist  

(* Used for printing actions from the domain.pddl *)

(* Helper function for parameters *)
let string_of_params params =
  "(" ^ String.concat " " params ^ ")"  

(* This function is a prettyprinter for the actions from the domain.pddl*)  
  let print_action a =
  print_endline ("(:action " ^ a.aname);
  print_endline (":parameters " ^ string_of_params a.parameters);
  print_endline (":precondition " ^ string_of_expr a.precondition);
  print_endline (":effect " ^ string_of_expr a.effects ^ ")");
  print_endline ""

(* This function iterates over the print_action function from earlier and creates a list *)
 let print_all_actions alist =
  List.iter print_action alist 

(* Newline function used in the print_program function *)  
let newline () = print_endline ""  

(* print_program is the main function *)
let print_program p =
  match p.defs with
  | DomainDef d ->
    print_endline ("(define");
    print_endline (Printf.sprintf "(domain %s)" d.domain.domain_name);
    print_endline ("(");
    print_endline (":requirements"); (* note: experiments with dot notation as with domain *)
    print_endline ":strips";
    print_endline "(:predicates";
    print_all_pdefinitions d.predicates;
    newline ();
    print_all_derived d.derived;
    newline ();
    print_all_actions d.actions;
    print_endline ")"; 
  | ProblemDef pd ->
    print_endline ("(define ");
    print_endline (Printf.sprintf "(problem %s)" pd.problem.problem_name);
    print_endline (Printf.sprintf "(:domain %s)" pd.problemdomain.problemdomain_name);
    print_endline("(:objects ");
    print_objects_decl pd.objects;
    print_endline(")");

    (* define end parenthesis *)
    print_endline (")"); 