
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
  | GridAndObjects of int * int * string * string list (* Grid AND normal objects *)

type node =
| Node of int * int

type rc =
| RowsColumns of string * int * int * string * int * int 

type entry = char

type row = 
| NormalRow of entry list
| MultRow of entry list * int

type key = {kname : string; shape : string; location : string}

type argument = 
| OnlyArguments of {a : string}
| GridArguments of int * int
| OpenNodesArgs of node list

type state = 
| OnlyStates of { sname : string; arguments : argument list}
| LockedNodesMatrix of { rows : row list; matrix_name : string; shape : state}
| LockedNodes of node list * state 
| OpenNodes of rc * state
| Keys of key list
| KeylocationMatrix of { rows : row list; }
| GridConnection of string list

type init = state list

type goal = expr

type domain_def = {
  domain : domain;
  requirements : requirements;
  predicates : pdefinition list;
  derived : derived list;
  actions : action list
}

type problem_def = { 
  problem : problem;
  problemdomain : problemdomain;
  objects : objects_decl;
  init : state list;
  goal : expr;}

(* the below code is for define *)
(* the parameters can be used only because they are derived above *)
type define =
  | DomainDef of domain_def
  | ProblemDef of problem_def

  (* This is our 'main' type. we need to put all the rest of the types in here*)
type program = {defs : define} (*in the parser we said that the program is "defs", so here we declare the type, which is define, which is declared above*)



(* type stmt = {
  name : string list;
} *)
