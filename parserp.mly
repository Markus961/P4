%{

  open Ast

%}

%token LPAREN RPAREN
%token DEFINE PROBLEM OBJECTS INIT GOAL
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
| LPAREN DEFINE p = problem d = domain o = objects i = init g = goal RPAREN (*this descibes how it looks in the domain file, domain, requirements, and predicates will be expanded below*)
    { { problem = p; domain = d; objects = o; init = i; goal = g}  }

problem:
| LPAREN PROBLEM problem_name = NAME RPAREN { { problem_name = problem_name } }

domain:
| LPAREN DOMAIN domain_name = NAME RPAREN { { domain_name = domain_name } } (*reasds domain part of domain file, and adds node to ast*)

(*  below, params gets defined as a list *)
requirements:
| LPAREN OBJECTS pa = params RPAREN
    { { oparams = pa } }
;

params:
| LPAREN OBJECTS ob = ob_list RPAREN
    { ob }
;

ob_list:
| { [] }
| o = odef rest = ob_list { o :: rest }
;

odef:
| name = NAME { { oname = name } } 
;


init:
| LPAREN INIT s = state_list RPAREN { { s } }
;

state_list:
| { [] }
| s = state rest = state_list { s :: rest }
;


state:
| LPAREN name = NAME args = arg_list RPAREN { { sname = name; arguments = args } } 
;

arg_list:
| { [] }
| a = argument rest = arg_list { a :: rest }
;