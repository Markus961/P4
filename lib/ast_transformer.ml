open Ast

let grid_to_strings rows cols grid_name =
  let acc = ref [] in
  if rows <= 0 || cols <= 0 then
    invalid_arg "row and cols must be positive";
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        acc := Printf.sprintf "%s%d-%d" grid_name i j :: !acc
      done;
    done;
  (* List is built in reverse so it must be reversed *)
  List.rev !acc

let transform_objects_decl obj =
  match obj with
  (* If NormalObjects do nothing *)
  | NormalObjects _ -> obj
  (* If GridAndObjects transform into NormalObjects *)
  | GridAndObjects (rows, cols, grid_name, objs) ->
    let grid_objs = grid_to_strings rows cols grid_name in
        NormalObjects (grid_objs @ objs)
(* takes gridAndObjects and put the variables into Some(or None if there is no grid) *)
let obj_grid_data_of_objects_decl = function
  | GridAndObjects (rows, cols, grid_name, _) -> Some (rows, cols, grid_name)
  | NormalObjects _ -> None

  (*Count the rows. each instance of normalRow - add 1 to the accumulator, return that int *)
let row_count rows =
  List.fold_left
    (fun acc row ->
      match row with
      | NormalRow _ -> acc + 1
      | MultRow _ ->
        failwith (Printf.sprintf "error"))
    0
    rows
    (*Count the cols. each instance of normalRow har a list, get that list.length, return that int *)
 let cols_count = function
  | NormalRow entries -> List.length entries
  | MultRow _ ->
      failwith (Printf.sprintf "error")

      (*here we check - same name, same row and cols int*)
let validate_locked_matrix expected_rows expected_cols expected_name rows matrix_name =
  if matrix_name <> expected_name then
    invalid_arg
      (Printf.sprintf
         "locked_nodes_matrix uses '%s' but objects grid name is '%s'"
         matrix_name
         expected_name);
  let actual_rows = row_count rows in
  if actual_rows <> expected_rows then
    invalid_arg
      (Printf.sprintf
         "locked_nodes_matrix has %d rows but objects grid expects %d"
         actual_rows
         expected_rows);
  List.iter
    (fun row ->
      let actual_cols = cols_count row in
      if actual_cols <> expected_cols then
        invalid_arg
          (Printf.sprintf
             "locked_nodes_matrix row has %d cols but objects grid expects %d"
             actual_cols
             expected_cols))
    rows


let rec string_of_arguments args = 
  match args with
  | [] -> []
  | OnlyArguments { a } :: tl ->
      a :: string_of_arguments tl
  | _ -> []

let string_of_state = function
  | OnlyStates { sname = _ ; arguments } -> 
    let args = string_of_arguments arguments in
      String.concat "" args
  | LockedNodesMatrix _ -> ""
  | LockedNodes _ ->  ""
  | OpenNodes _ -> ""
  | Keys _ -> ""
  | KeylocationMatrix _ -> ""
  | GridConnection _ -> ""

let matrix_to_nodes rows matrix_name shape =
  List.concat (
    List.mapi (fun i row ->
    match row with
    | NormalRow entries ->
      List.concat (
        List.mapi (fun j entry ->
          match entry with
            | "0" ->
              [OnlyStates {
                sname = "open"; 
                arguments = [OnlyArguments {a = Printf.sprintf "%s%d-%d" matrix_name i j}]}
              ]
            | _ ->
              let arg = Printf.sprintf "%s%d-%d" matrix_name i j in
                [OnlyStates {
                  sname = "locked"; 
                  arguments = [OnlyArguments {a = arg}]
                };
                OnlyStates {
                  sname = "lock-shape"; 
                  arguments = [
                    OnlyArguments {a = arg}; 
                    OnlyArguments {a = string_of_state shape}
                  ]
                }
                ]

      ) entries
      )
      
    | (* MultRow (entries, multiplicator) *) _ ->
      failwith (Printf.sprintf "error")
    ) rows
  )

let rec transform_init obj_grid_data (states : state list) =
  match states with
  | [] -> []
  | (OnlyStates _ as s) :: tl -> 
      s :: transform_init obj_grid_data tl
  | (LockedNodesMatrix { rows; matrix_name; shape }) :: tl ->
  (match obj_grid_data with
    | Some (expected_rows, expected_cols, expected_name) -> (*compare lockedNodeMatrix with gridobj. *)
      validate_locked_matrix expected_rows expected_cols expected_name rows matrix_name
    | None ->
      invalid_arg "locked_nodes_matrix doesn't match :objects (:grid ...)");  
      matrix_to_nodes rows matrix_name shape @ transform_init obj_grid_data tl
  (* | LockedNodes (nodes, st) as hd :: tl ->
      hd :: transform_init tl
  | OpenNodes (rc, st) as hd :: tl ->
      hd :: transform_init tl
  | Keys keys as hd :: tl ->
      hd :: transform_init tl
  | KeylocationMatrix { rows } as hd :: tl ->
      hd :: transform_init tl
  | GridConnection flags as hd :: tl ->
      hd :: transform_init tl *)
  | _ ->
      failwith ("Hi failure")

let transform_program p =
  match p.defs with
  (* We didn't provide extensions for domain.pddl so nothing happens *)
  | DomainDef _ -> p
  | ProblemDef problem_def ->
      let problem = problem_def.problem in
      let problemdomain = problem_def.problemdomain in
      let new_objects = transform_objects_decl problem_def.objects in
      let grid = problem_def.grid in
      let init = problem_def.init in (* Make transform_init *)
      (*let obj_grid_data = obj_grid_data_of_objects_decl problem_def.objects in
      let new_init = transform_init obj_grid_data problem_def.init in  Make transform_init *)
      let goal = problem_def.goal in
{
  defs =
    ProblemDef {
      problem;
      problemdomain;
      objects = new_objects;
      grid;
      init;
      goal;
    }
}
