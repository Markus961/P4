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
  | NormalObjects _ -> obj
  | GridAndObjects (rows, cols, objs) ->
    let grid_objs = grid_to_strings rows cols in
        NormalObjects (grid_objs @ objs)
   

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