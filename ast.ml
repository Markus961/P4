
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

type expr = 
| Epdef of pdefinition
| Eand of expr list
| Eexists of variable list * expr

type derived = {header : pdefinition; body : expr}

(* the below code is for define *)
(* the parameters can be used only because they are derived above *)
type define = {
  domain : domain; 
  requirements : requirements; 
  predicates : pdefinition list; 
  derived : derived list}

(* This is our 'main' type. we need to put all the rest of the types in here*)
type program = {defs : define}


