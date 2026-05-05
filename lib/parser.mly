%{

  open Ast

%}

%token DEFINE DOMAIN REQUIREMENTS STRIPS
%token PREDICATES ACTION PARAMETERS PRECONDITION EFFECT
%token INIT OBJECTS PROBLEM PROBLEMDOMAIN GOAL GRID LOCKEDNODESMATRIX  KEYLOCATIONMATRIX
(*%token LOCKEDNODES OPENNODES DASH -- Unused atm*)
%token ROWS COLUMNS GRIDNAME CONNECTIONS KEYS SHAPES

%token AND EXISTS NOT PLUS MULT COMMA EQUALS
%token LPAREN RPAREN LBRACKET RBRACKET 
%token <int> CONST
%token <string> VAR
%token <string> NAME
%token <string> FLAG
%token EOF

(* Here the program starts *)
%start prog

(* Here we parse the values from the syntacs tree *)
%type <Ast.program> prog

%%
(* grammar rules *)
(* the program starts by evaluating define *)
prog:
| defs = define EOF (*men det program start it reads the define block in the domain file, and when its done with it, it is done=EOF*)
{ {defs = defs} } (*record, is used for ast. The purple bracket is a node/ocaml record for our ast*)
;

(* domain = d, requirements = r, predicates = p etc.. are children of define *)
(* therefore we make an ocaml record/datastructure to store them *)
(* this corresponds to a tree where parent is define and domain etc. are children *)
define:
(*this descibes how it looks in the domain file, domain, requirements, and predicates will be expanded below*)
| LPAREN DEFINE d = domain r = requirements p = predicates declarations = declaration_list RPAREN
     { DomainDef { domain = d; requirements = r; predicates = p; actions = declarations } }
| LPAREN DEFINE p = problem pd = problemdomain o = objects grid = grid i = init g = goal RPAREN 
    { ProblemDef { problem = p; problemdomain = pd; objects = o; grid = grid; init = i; goal = g}  }
;

declaration_list:  
  | { [] }   
  | a = action rest = declaration_list { a :: rest }
    (*hvis vi møder action: tilføjer a foran rest*)



domain:
| LPAREN DOMAIN name = NAME RPAREN { { domain_name = name } } (*reasds domain part of domain file, and adds node to ast*)


(*  below, params gets defined as a list *)
requirements:
| LPAREN REQUIREMENTS f = params RPAREN
    { { features = f } }
;

(* Parse  requirement features into a list(lst). *)
params:
| lst = feature_list { lst }

feature_list:
| { [] }
| f = features rest = feature_list { f :: rest }

features: 
| STRIPS {Strips} 
;

predicates:
| LPAREN PREDICATES pdefs = pdefinition_list RPAREN
    { pdefs }
;

pdefinition_list:
| { [] }
| d = pdefinitions rest = pdefinition_list { d :: rest }
;

pdefinitions:
| LPAREN name = NAME vars = variable_list RPAREN { { pname = name; variables = vars } } 
;

variable_list:
| { [] }
| v = variable rest = variable_list { v :: rest }
;

variable:
| v = VAR {v} (* VAR means "?", id is the variable's name *)
;

action:
| LPAREN ACTION name = NAME
    PARAMETERS LPAREN params = variable_list RPAREN
    PRECONDITION pre = expr
    EFFECT eff = expr
  RPAREN
  {
    {
      aname = name;
      parameters = params;
      precondition = pre;
      effects = eff;
    }
  }
;

expr:
| LPAREN AND e = expr_list RPAREN { And e }
| LPAREN NOT e = expr RPAREN { Not e }
| LPAREN EXISTS LPAREN vl = variable_list RPAREN e = expr RPAREN { Exists (vl, e) }
| LPAREN name = NAME args = term_list RPAREN { Atom (name, args) }
;

expr_list:
| { [] }
| e = expr rest = expr_list { e:: rest }
;

term_list:
| { [] }
| t = term rest = term_list { t :: rest }
;

term:
| v = VAR { v }
| n = NAME { n }
;


(*Parsing for Problem File*)
problem:
| LPAREN PROBLEM problem_name = NAME RPAREN { { problem_name = problem_name } }
;

problemdomain:
| LPAREN PROBLEMDOMAIN problemdomain_name = NAME RPAREN { { problemdomain_name = problemdomain_name } }
;

objects:
| LPAREN OBJECTS ob = ob_list RPAREN { NormalObjects ob }
(*| LPAREN OBJECTS LPAREN GRID rows = CONST columns = CONST grid_name = NAME RPAREN ob = ob_list RPAREN { GridAndObjects (rows, columns, grid_name, ob) }
| LPAREN OBJECTS ob = ob_list LPAREN GRID rows = CONST columns = CONST grid_name = NAME RPAREN RPAREN { GridAndObjects (rows, columns, grid_name, ob) }*)
;

grid:
| LPAREN GRID gas = gridargs RPAREN { build_grid gas }
;

(* Filled backwards to avoid ambiguity, shouldn't mess up grid arguments in the AST *)
gridargs:
| { [] }
| tl = gridargs ga = gridarg { ga :: tl }
;

gridarg:
| ROWS rows = CONST { GP_rows rows }
| COLUMNS cols = CONST { GP_cols cols }
| GRIDNAME n = NAME { GP_name n }

| CONNECTIONS fl = flag_list { GP_connections fl }
| KEYS ns = simple_name_list {GP_key_names ns }
| SHAPES LPAREN sl = shape_mapping_list RPAREN { GP_shapes sl }
| LOCKEDNODESMATRIX ln = NAME LPAREN LBRACKET r = grid_rows RBRACKET s = state RPAREN { GP_lnm (LockedNodesMatrix { matrix_name = ln; rows = r; shape = s }) }
| KEYLOCATIONMATRIX km = NAME LPAREN LBRACKET r = grid_rows RBRACKET RPAREN { GP_klm (KeylocationMatrix { matrix_name = km; rows = r }) }
;

simple_name_list:
| n = NAME { [n] }
| n = NAME rest = simple_name_list { n :: rest }
;

shape_mapping_list:
| { [] }
| m = shape_mapping rest = shape_mapping_list { m :: rest }
;

shape_mapping:
| LPAREN id = NAME EQUALS s = NAME RPAREN {{ char_id = id; shape_name = s } }
;

ob_list:
| n = NAME { [n] }
| n = NAME ob_list_tail = ob_list { n :: ob_list_tail }
;


init:
| LPAREN INIT s = state_list RPAREN { s }
;

state_list:
| { [] }
| s = state rest = state_list { s :: rest }
;

state:
| LPAREN name = NAME args = arg_list RPAREN { OnlyStates { sname = name; arguments = args } } 
(*| l = locked_nodes_matrix { l }
| ln = locked_nodes { ln }
| o = open_nodes { o }
| k = keys { k }
| km = keylocation_matrix { km }
| gc = gridconnection { gc }*)
;

arg_list:
| { [] }
| a = argument rest = arg_list { a :: rest }
;

argument:
| a = NAME { OnlyArguments { a } }
| GRID i1 = CONST i2 = CONST { GridArguments ( i1, i2 ) }
| LBRACKET n = node_list RBRACKET { OpenNodesArgs ( n ) }
;



(*SKAL NOK BRUGES SENERE MEN GIVER WARNING NU, ALLE 5 locked_nodes til node*)

(*locked_nodes:
| LPAREN LOCKEDNODES LBRACKET n = node_list RBRACKET s = state RPAREN { LockedNodes ( n, s ) }
;*)

(*open_nodes:
| LPAREN OPENNODES LPAREN rc = rc_list RPAREN s = state RPAREN { OpenNodes ( rc, s ) }
;

rc_list:
| rows = NAME r1 = CONST DASH r2 = CONST cols = NAME c1 = CONST DASH c2 = CONST { RowsColumns ( rows, r1, r2, cols, c1, c2 ) }
;*)


node_list:
| { [] }
| n = node rest = node_list { n :: rest}
;

node:
| LPAREN i1 = CONST COMMA i2 = CONST RPAREN { Node ( i1, i2 ) }
;

(*locked_nodes_matrix:
| LPAREN LOCKEDNODESMATRIX LBRACKET r = rows RBRACKET s = state RPAREN { LockedNodesMatrix { rows = r; shape = s } }
;*)

(*gridconnection:
| LPAREN GRIDCONNECTION fl = flag_list RPAREN { GridConnection fl }
;*)

flag_list:
| { [] }
| f = flag rest = flag_list { f :: rest }
;

flag:
| f = FLAG { f }
;

(*keylocation_matrix:
| LPAREN KEYLOCATIONMATRIX LBRACKET r = rows RBRACKET RPAREN { KeylocationMatrix { rows = r } }
;*)

(* supports use of classic matrix notation OR mult-notation but not both simultaneousely *)
grid_rows:
| { [] }
| r = row rest = grid_rows { r :: rest }
| m = repeat_notation_option rest = grid_rows {print_endline "Hit repeat_notation_option"; m :: rest }
;

repeat_notation_option:
| p = row_part { MultRowOption [p] }
| p = row_part PLUS m = repeat_notation_option { 
  print_endline "Hit MultRowOption"; 
  (match m with
  | MultRowOption lst -> MultRowOption (p :: lst)
  | _ -> assert false)
}
;

row_part:
| LBRACKET en = entries RBRACKET MULT n = CONST { print_endline "Hit MultRow"; MultRow (en, n) }
;

row:
| LBRACKET en = entries RBRACKET { NormalRow (en) }
;

entries:
| { [] }
| ent = entry rest = entries { ent :: rest }
;

entry:
| c = CONST { string_of_int(c) }
| n = NAME { n }
;


goal:
| LPAREN GOAL e = expr RPAREN { e }
;