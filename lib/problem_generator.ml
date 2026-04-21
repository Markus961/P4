open Ast
(* open Ast_transformer *)

let string_of_objects_decl obj =
  match obj with
  | NormalObjects objs ->
      String.concat " " objs
  | _ -> ""

(* For printing OnlyStates *)
let string_of_argument args =
  match args with
  | OnlyArguments { a } ->
      a
  | _ -> ""

let string_of_onlystates sname arguments =
   "(" ^ sname ^ " " ^ String.concat " " (List.map string_of_argument arguments) ^ ")"

let rec string_init_section state_list =
  match state_list with
  | [] -> ""
  | OnlyStates { sname; arguments } :: tl ->
      string_of_onlystates sname arguments ^ "\n"
      ^ string_init_section tl
  | _ -> ""



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

let string_of_goal goal =
  string_of_expr goal



let string_of_problem p =
  "(define (problem " ^ p.problem.problem_name ^ ")\n"
  ^ "(:domain " ^ p.problemdomain.problemdomain_name ^ ")\n"
  ^ "(:objects " ^ string_of_objects_decl p.objects ^ ")\n"
  ^ "(:init " ^ string_init_section p.init ^ ")\n"
  ^ "(:goal " ^ string_of_goal p.goal ^ ")\n"
  ^ ")" 