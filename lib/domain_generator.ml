open Ast
(* open Ast_transformer *)


(* Used for printing predicate definitions from domain.pddl *)
let string_of_pdefinition pdefs =
  "(" ^ pdefs.pname ^ " " ^ String.concat " " pdefs.variables ^ ")"

let string_of_all_pdefinitions preds =
  String.concat "\n" (List.map string_of_pdefinition preds)


(*turn features into strings*)
let string_of_feature f =
  match f with
  | Strips -> ":strips"
  | DerivedPredicates -> ":derived-predicates"

let string_of_requirements req =
  "(:requirements " ^ String.concat " " (List.map string_of_feature req.features) ^")"


let string_of_domain d =
  "(define (domain " ^ d.domain.domain_name ^ ")\n"
  ^ string_of_requirements d.requirements ^ "\n"
  ^ "(:predicates " 
  ^ string_of_all_pdefinitions d.predicates ^ "\n"
  ^ ")\n)"