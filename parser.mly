%{

  open Ast

%}

%token LPAREN RPAREN
%token DEFINE DOMAIN REQUIREMENTS DPREDICATES STRIPS
%token PREDICATES ACTION PARAMETERS PRECONDITION EFFECT
%token AND NOT
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

(* domain = d, requirements = r, predicates = p are children of define *)
(* therefore we make an ocaml record/datastructure to store them *)
(* this corresponds to a tree where parent is define and domain etc. are children *)
define:
| LPAREN DEFINE d = domain r = requirements p = predicates a = action_list RPAREN (*this descibes how it looks in the domain file, domain, requirements, and predicates will be expanded below*)
    { { domain = d; requirements = r; predicates = p; actions = a }  } (*makes three nodes in ast, called, domain, requirements, predicates, which will be expanded, which means they are not terminal, they have more nodes below them*)
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

action_list:
| { [] }
| a = action rest = action_list { a:: rest }
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
| LPAREN AND e1 = expr_list RPAREN { And e1 }
| LPAREN NOT e = expr RPAREN { Not e }
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