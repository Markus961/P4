open Ast

let grid_to_strings rows cols =
  let acc = ref [] in
    for i = 0 to rows - 1 do
      for j = 0 to cols -1 do
        acc := Printf.sprintf "node%d-%d" i j :: !acc
      done
    done;
  (* List is built in reverse so it must be reversed *)
  List.rev !acc

let transform_objects_decl obj =
  match obj with
  (* If NormalObjects do nothing *)
  | NormalObjects _ -> obj
  (* If GridAndObjects transform into NormalObjects *)
  | GridAndObjects (rows, cols, objs) ->
    let grid_objs = grid_to_strings rows cols in
        NormalObjects (grid_objs @ objs)


let transform_program p =
  match p.defs with
  (* We didn't provide extensions for domain.pddl so nothing happens *)
  | DomainDef _ -> p
  | ProblemDef problem_def ->
      let problem = problem_def.problem in
      let problemdomain = problem_def.problemdomain in
      let new_objects = transform_objects_decl problem_def.objects in
      let init = problem_def.init in
      let goal = problem_def.goal in
{
  defs =
    ProblemDef {
      problem;
      problemdomain;
      objects = new_objects;
      init;
      goal;
    }
}
   

(*
let transform_argument arg =
  assert false
*)



(*
let transform_state state =
  assert false
*)


  (* i main: i main: 
  let parsed = Parser.problem Lexer.token lexbuf in 
  let transformed = Transform.transform_program parsed in transformed *)