
(* the below code is for domain *)
type domain = { domain_name : string}

(* the below code is for requirements *)
type feature =
| Strips
| DerivedPredicates

type requirements = {	features : feature list}


(* the below code is for :predicates *)
type variable = string

type pdefinition = {pname : string; variables : variable list} 


(* the below code is for define *)
(* the parameters can be used only because they are derived above *)
type define = { domain : domain; requirements : requirements; predicates : pdefinition list}

(* This is our 'main' type. we need to put all the rest of the types in here*)
type program = {defs : define}
(*
type symbol = {name : string;}

type predicates = {
  pred_name : string;
	pred_params : symbol list;
} 

type derived = { 
  derived_conditions : predicates;
  derived_stmt : string list;
}

type stmt = {
  name : string list;
} *)

