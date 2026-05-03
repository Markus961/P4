open Ast
(* open Ast_transformer *)


(* Used for printing predicate definitions from domain.pddl *)
let string_of_pdefinition pdefs =
  "(" ^ pdefs.pname ^ " " ^ String.concat " " pdefs.variables ^ ")"

let string_of_all_pdefinitions preds =
  "(:predicates " ^ String.concat " " (List.map string_of_pdefinition preds) ^ ")\n"


(*turn features into strings*)
let string_of_feature f =
  match f with
  | Strips -> ":strips"

let string_of_requirements req =
  "(:requirements " ^ String.concat " " (List.map string_of_feature req.features) ^ ")\n"


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



(* Used for printing actions from the domain.pddl *)

(* Helper function for parameters *)
let string_of_params params =
  "(" ^ String.concat " " params ^ ")"  

(* This function is a prettyprinter for the actions from the domain.pddl*)  
  let string_of_action a =
  "(:action " ^ a.aname ^ " "
  ^ ":parameters " ^ string_of_params a.parameters ^ " "
  ^ ":precondition " ^ string_of_expr a.precondition ^ " "
  ^ ":effect " ^ string_of_expr a.effects
  ^ ")\n"

(* This function iterates over the print_action function from earlier and creates a list *)
 let string_of_all_actions alist =
  String.concat "\n" (List.map string_of_action alist)



let string_of_domain d =
  "(define (domain " ^ d.domain.domain_name ^ ")\n"
  ^ string_of_requirements d.requirements ^ "\n"
  ^ string_of_all_pdefinitions d.predicates ^ "\n"
  ^ string_of_all_actions d.actions ^ "\n"
  ^ ")\n"