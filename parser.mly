%{

  open Ast

%}

%token LPAREN RPAREN
%token DEFINE REQUIREMENTS DPREDICATES STRIPS
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
prog:
| defs = define; main = requirements
EOF 
{ {defs = defs; main = main} }
;

(* Parse  requirement features into a list(lst). *)
params:
| lst = list(features) {lst}
;

features: 
| STRIPS {Strips} 
| DPREDICATES {DerivedPredicates}
;

(* _  before 'keyword' tells ocaml 'we dont need this for anything' 
we will hovever need 'domain name' for connection to problem.pddl later on*)
define:
| LPAREN; DEFINE; LPAREN; _keyword = NAME; name = NAME; RPAREN; RPAREN 
    { { domain_name = name }  }
;

(*  above, params gets defined as a list, sepereted by ':' *)
requirements:
| LPAREN; REQUIREMENTS; features = params; RPAREN
    { { features = features } }
;

predicates:
| LPAREN; PREDICATES; pdefinitions; RPAREN
    { pdefinitions }
;

pdefinitions:
| LPAREN NAME vars = variable_list RPAREN {pname = name; variables = vars} 
;

variable_list:
| {[]}
| variable variable_list { variable :: variable_list }
;

variable:
| VAR {VAR id} (* VAR means "?", id is the variable's name *)
;




