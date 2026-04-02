
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
| Atom of string * string list (* contains predicate name + terms *)
| And of expr list
| Not of expr
| Exists of variable list * expr

type derived = {header : pdefinition; body : expr}

type action = { aname : string; parameters : variable list; precondition : expr; effects : expr; }

(*Problem File AST*)

type problem = {problem_name : string;}

type problemdomain = {problemdomain_name : string;}

type odef = { oname : string }

type objects_decl =
  | NormalObjects of string list (* Only objects: triangle diamond key1 etc. *)
  | GridAndObjects of int * int * string list (* Grid AND normal objects *)

type argument = 
| OnlyArguments of {a : string}
| GridArguments of int * int

type row = entry list

and entry = char

type key = {kname : string; shape : string; location : string}

type state = 
| OnlyStates of { sname : string; arguments : argument list}
| LockedNodes of { rows : row list; shape : state}
| Keys of key list

type init = state list

type goal = expr

(* the below code is for define *)
(* the parameters can be used only because they are derived above *)
type define =
  | DomainDef of {
  domain : domain; 
  requirements : requirements; 
  predicates : pdefinition list; 
  derived : derived list;
  actions : action list
  }
  | ProblemDef of { problem : problem;
  problemdomain : problemdomain;
  objects : objects_decl;
  init : state list;
  goal : expr;}

  (* This is our 'main' type. we need to put all the rest of the types in here*)
type program = {defs : define} (*in the parser we said that the program is "defs", so here we declare the type, which is define, which is declared above*)



(* type stmt = {
  name : string list;
} *)
