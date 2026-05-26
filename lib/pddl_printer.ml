open Ast
(* open Ast_transformer *)

let print_objects_decl obj =
  match obj with
  | NormalObjects objs ->
      List.iter print_endline objs
  | _ -> 
    print_endline ("No objects")

(* For printing OnlyStates *)
let print_argument args =
  match args with
  | OnlyArguments { a } ->
      print_endline a
  | _ -> ()

let print_onlystates sname arguments =
    print_endline ("(" ^ sname);
    List.iter print_argument arguments;
    print_endline (")")

let rec print_init_section state_list =
  match state_list with
  | [] -> ()
  | OnlyStates { sname; arguments } :: tl ->
      print_onlystates sname arguments;
      print_init_section tl
  | _ -> ()
(* Used for printing predicate definitions from domain.pddl *)
let print_pdefinition pdefs =
  print_endline ("(" ^ pdefs.pname);
  List.iter print_endline pdefs.variables;
  print_endline (")")

let print_all_pdefinitions pdefinitions =
  List.iter print_pdefinition pdefinitions



(*turn features into strings*)
let string_of_feature f =
  match f with
  | Strips -> ":strips"

let print_requirements req =
  List.iter (fun f -> print_endline (string_of_feature f)) req.features


(* string_of_expr is a function that takes the ast expr and translates it to a string *)  
  let rec string_of_expr expr =
  match expr with
  | Atom (name, args) ->
    "(" ^ name ^ " " ^ String.concat " " args ^ ")" (*String.concat takes string list (args), return one combined string, with elements from the string list, and has a seperator (with is space)*)
  | And exprs ->
    "(and " ^ String.concat " " (List.map string_of_expr exprs) ^ ")"
  | Not expr ->
    "(not " ^ string_of_expr expr ^ ")"
  | Exists (vars, expr) ->
    "(exists (" ^ String.concat " " vars ^ ") " ^ string_of_expr expr ^ ")"

let print_goal goal =
  print_endline (string_of_expr goal) 


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


(* print_program is the main function *)
let print_program p =
  match p.defs with
  | DomainDef d ->
    print_endline ("(define");
    print_endline (Printf.sprintf "(domain %s)" d.domain.domain_name);
    print_endline ("(:requirements"); 
    print_requirements d.requirements;
    print_endline (")");
    print_endline "(:predicates";
    print_all_pdefinitions d.predicates;
    print_endline ("\n");
    print_endline ("\n");
    print_all_actions d.actions;
    print_endline ")"; 
  | ProblemDef pd ->
    print_endline ("(define ");
    print_endline (Printf.sprintf "(problem %s)" pd.problem.problem_name);
    print_endline (Printf.sprintf "(:domain %s)" pd.problemdomain.problemdomain_name);
    print_endline("(:objects ");
    print_objects_decl pd.objects;
    print_endline (")");
    print_endline ("(:init ");
    print_init_section pd.init;
    print_endline (")");
    print_endline ("(:goal");
    print_goal pd.goal;
    print_endline (")");
    
    (* define end parenthesis *)
    print_endline (")"); 