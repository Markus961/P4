

type define = { domain_name : string;}


type requirements = {	features : string list;}

(* This is our 'main' type. we need to put all the rest of the types in here*)
type program = {defs : define; main : requirements}
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

