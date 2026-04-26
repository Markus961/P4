
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

type entry = string

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

(* type stmt = {
  name : string list;
} *)

type grid_param =
  | GP_rows of int
  | GP_cols of int
  | GP_name of string
  | GP_connections of string list
  | GP_keys of key list
  | GP_lnm of state
  | GP_klm of state

type grid = {
  rows : int option;
  cols : int option;
  name : string option;
  connections : string list;
  keys : key list;
  locked : state option;
  keyloc : state option;
}

let build_grid (params : grid_param list) : grid =
  let rows = ref None in
  let cols = ref None in
  let name = ref None in
  let connections = ref [] in
  let keys = ref [] in
  let locked = ref None in
  let keyloc = ref None in

  let set_once what r v =
    match !r with
    | None -> r := Some v
    | Some _ -> failwith ("Duplicate grid field: " ^ what)
  in

  List.iter
    (function
      | GP_rows r -> set_once "rows" rows r
      | GP_cols c -> set_once "columns" cols c
      | GP_name n -> set_once "name" name n
      | GP_connections cs -> connections := cs
      | GP_keys ks -> keys := ks
      | GP_lnm l -> set_once "lockedlocations" locked l
      | GP_klm k -> set_once "keylocations" keyloc k
    )
    params;

  {
    rows = !rows;
    cols = !cols;
    name = !name;
    connections = !connections;
    keys = !keys;
    locked = !locked;
    keyloc = !keyloc;
  }


  (* the below code is for define *)
(* the parameters can be used only because they are derived above *)
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
  grid : grid;
  init : state list;
  goal : expr;}

(* the below code is for define *)
(* the parameters can be used only because they are derived above *)
type define =
  | DomainDef of domain_def
  | ProblemDef of problem_def

  (* This is our 'main' type. we need to put all the rest of the types in here*)
type program = {defs : define} (*in the parser we said that the program is "defs", so here we declare the type, which is define, which is declared above*)