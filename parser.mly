%{

  open Ast

%}

%token LPAREN RPAREN COLON
%token DEFINE REQUIREMENTS
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

(* Parse  requirement names (seperated by COLON)  into a list(lst). *)
params:
| lst = separated_list(COLON, n = NAME {n}) {lst}


(* _  before 'keyword' tells ocaml 'we dont need this for anything' 
we will hovever need 'domain name' for connection to problem.pddl later on*)
define:
| LPAREN; DEFINE; LPAREN; _keyword = NAME; name = NAME; RPAREN; RPAREN 
    { { domain_name = name }  }

(*  above, params gets defined as a list, sepereted by ':' *)
requirements:
| LPAREN; COLON; REQUIREMENTS; COLON; features = params; RPAREN
    { { features = features } }



