open Ast
(* open Ast_transformer *)

let print_objects_decl obj =
  match obj with
  | NormalObjects objs ->
      List.iter print_endline objs

  | GridAndObjects (r, c, objs) ->
      Printf.printf "GridAndObjects (%d x %d)\n" r c;
      List.iter print_endline objs

let print_program p =
  match p.defs with
  | DomainDef d ->
    print_endline ("(define");
    print_endline (Printf.sprintf "(domain %s)" d.domain.domain_name);
    print_endline ("(");
    print_endline (":requirements"); (* note: experiments with dot notation as with domain *)
    print_endline ":strips";
    print_endline ":derived-predicates";
    print_endline (")");
    
    print_endline (")");
  | ProblemDef pd ->
    print_endline ("(define ");
    print_endline (Printf.sprintf "(problem %s)" pd.problem.problem_name);
    print_endline (Printf.sprintf "(:domain %s)" pd.problemdomain.problemdomain_name);
    print_endline("(:objects ");
    print_objects_decl pd.objects;
    print_endline(")");

    (* define end parenthesis *)
    print_endline (")"); 