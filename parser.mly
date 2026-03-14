%{

  open Ast

%}

%token LPAREN RPAREN
%token DEFINE DOMAIN REQUIREMENTS DPREDICATES STRIPS
%token PREDICATES 
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
| defs = define;
EOF 
{ {defs = defs} }
;

(* domain = d, requirements = r, predicates = p are children of define *)
(* therefore we make an ocaml record/datastructure to store them *)
(* this corresponds to a tree where parent is define and domain etc. are children *)
define:
| LPAREN DEFINE d = domain r = requirements p = predicates RPAREN 
    { { domain = d; requirements = r; predicates = [p] }  }
;

domain:
| LPAREN DOMAIN name = NAME RPAREN { { domain_name = name } }

(*  below, params gets defined as a list *)
requirements:
| LPAREN; REQUIREMENTS; f = params; RPAREN
    { { features = f } }
;

(* Parse  requirement features into a list(lst). *)
params:
| lst = list(features) {lst}
;

features: 
| STRIPS {Strips} 
| DPREDICATES {DerivedPredicates}
;

predicates:
| LPAREN; PREDICATES; pdefinitions = pdefinitions; RPAREN
    { pdefinitions }
;

pdefinitions:
| LPAREN name = NAME vars = variable_list RPAREN { { pname = name; variables = vars } } 
;

variable_list:
| lst = list(variable) {lst}
;

variable:
| v = VAR {v} (* VAR means "?", id is the variable's name *)
;





