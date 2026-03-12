%{

  open Ast

%}

%token LPAREN RPAREN
%token DASH
%token COLON
%token QUESTIONMARK
%token ACTION PARAMETERS PRECONDITION EFFECT
%token NOT 
%token <string> NAME

(* Here the program starts *)
%start prog

(* Here we parse the values from the syntacs tree *)
%type <Ast.program> prog

%%

(* The rules of the grammar *)

action:
  | LPAREN ACTION name = NAME action_list RPAREN { Action name }
  | PARAMETERS LPAREN object_list RPAREN  { Parameters $3 } 
;
action_list:
  | action_list { $1 :: $2 }
  | 
