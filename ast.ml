
(* the below code is for domain *)
type domain = { domain_name : string}

(* the below code is for requirements *)
type feature =
| Strips
| DerivedPredicates

type requirements = {	features : feature list} (*requirements contains features, fetures is a feature list*)


(* the below code is for :predicates *)
type variable = string

type pdefinition = {pname : string; variables : variable list} (*pdefintion has a name, which is a string, and it contains variables, which is a list of variables*)
(* Logical expressions for preconditions/effects *)
type expr =
 | Atom of string * string list (* (pred arg1 arg2 ...) *)
 | Not of expr
 | And of expr list

type action = { aname : string; parameters : variable list; precondition : expr; effects : expr; }

(* the below code is for define *)
(* the parameters can be used only because they are derived above *)
type define = { domain : domain; requirements : requirements; predicates : pdefinition list; actions : action list;}

(* This is our 'main' type. we need to put all the rest of the types in here*)
type program = {defs : define} (*in the parser we said that the program is "defs", so here we declare the type, which is define, which is declared above*)
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

