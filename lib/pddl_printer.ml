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
(*| LockedNodesMatrix { rows; shape } ->
      ()
  
  | LockedNodes { ; state } ->
      ()
  | OpenNodes ->
      ()
  | Keys ->
      ()
  | KeylocationMatrix ->
      ()
  | GridConnection ->
      () *)
  | _ -> ()
(* Used for printing predicate definitions from domain.pddl *)
let print_pdefinition pdefs =
  print_endline ("(" ^ pdefs.pname);
  List.iter print_endline pdefs.variables;
  print_endline (")")

let print_all_pdefinitions pdefinitions =
  List.iter print_pdefinition pdefinitions


let print_program p =
  match p.defs with
  | DomainDef d ->
    print_endline ("(define");
    print_endline (Printf.sprintf "(domain %s)" d.domain.domain_name);
    print_endline ("(");
    print_endline (":requirements"); (* note: experiments with dot notation as with domain *)
    print_endline ":strips";
    print_endline ":derived-predicates";
    print_endline "(:predicates";
    print_all_pdefinitions d.predicates;
    print_endline (")");
    
    print_endline (")");
  | ProblemDef pd ->
    print_endline ("(define ");
    print_endline (Printf.sprintf "(problem %s)" pd.problem.problem_name);
    print_endline (Printf.sprintf "(:domain %s)" pd.problemdomain.problemdomain_name);
    print_endline("(:objects ");
    print_objects_decl pd.objects;
    print_endline (")");
    print_endline ("(:init ");
    print_init_section pd.init;


    (* define end parenthesis *)
    print_endline (")"); 