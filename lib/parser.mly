%{

  open Ast

%}

%token DEFINE DOMAIN REQUIREMENTS DPREDICATES STRIPS
%token PREDICATES DERIVED ACTION PARAMETERS PRECONDITION EFFECT
%token INIT OBJECTS PROBLEM PROBLEMDOMAIN GOAL GRID LOCKEDNODES
%token AND EXISTS NOT 
%token LPAREN RPAREN LBRACKET RBRACKET
%token <int> CONST
%token <string> VAR
%token <string> NAME
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
(*makes three nodes in ast, called, domain, requirements, predicates, which will be expanded, which means they are not terminal, they have more nodes below them*)
    { let (derived_list, action_list) = declarations in DomainDef { domain = d; requirements = r; predicates = p; derived = derived_list; actions = action_list}  }
| LPAREN DEFINE p = problem pd = problemdomain o = objects i = init g = goal RPAREN 
    { ProblemDef { problem = p; problemdomain = pd; objects = o; init = i; goal = g}  }
;

declaration_list:  // parsing derived + actions in same grammar. removes ambiguity som gav error før, fordi de lignede hinanden for meget
  | { ([], []) }   
  | one_derived = derived rest = declaration_list { let (derived_list, action_list) = rest in (one_derived :: derived_list, action_list) }
    (*hvis vi møder derived: tilføjer d foran de-listen, beholder a-listen*)
  | one_action = action rest = declaration_list { let (derived_list, action_list) = rest in (derived_list, one_action :: action_list) }
    (*hvis vi møder action: tilføjer a foran a_list-listen, beholder de-listen*)

derived:
| LPAREN DERIVED h = pdefinitions b = expr  RPAREN { { header = h; body = b } }
;

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
| DPREDICATES {DerivedPredicates}
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
| LPAREN OBJECTS GRID rows = CONST columns = CONST ob = ob_list RPAREN { GridAndObjects (rows, columns, ob) }
| LPAREN OBJECTS ob = ob_list GRID rows = CONST columns = CONST RPAREN { GridAndObjects (rows, columns, ob) }
;

(*oparams:
| LPAREN ob = ob_list RPAREN
    { ob }
;*)

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
| LPAREN name = NAME args = arg_list RPAREN { OnlyStates {sname = name; arguments = args} } 
| ln = locked_nodes { ln }
;

locked_nodes:
| LPAREN LOCKEDNODES LBRACKET rlist = row_list RBRACKET s = state RPAREN { LockedNodes {rows = rlist; statement = s} } (* locked_nodes matrix returns a list of rows and a statement *)
;

row_list:
| { [] }
| r = row rest = row_list { r :: rest }
;

row:
| LBRACKET elist = entry_list RBRACKET { elist }
;

entry_list:
| { [] }
| e = entry rest = entry_list { e :: rest }
;

entry:
| num = CONST { IntEntry (num) }
| shape = NAME { StringEntry (shape) }
;

arg_list:
| { [] }
| a = argument rest = arg_list { a :: rest }
;

argument:
| a = NAME {a}
;

goal:
| LPAREN GOAL e = expr RPAREN { e }
;