open Ast
  
let generate_connections flags rows cols grid_name =
  match rows, cols, grid_name with
  | Some r, Some c, Some n ->
    List.concat (
      List.map (fun f ->
        match f with
        | "-H" -> Utils.generate_horizontal n r c
        | "-V" -> Utils.generate_vertical n r c
        | _ ->
          failwith "Invalid flag"
      ) flags
    )
  | _ ->
    failwith "Missing parameters (rows, columns, grid_name)"
    
let transform_grid_to_connections grid =
  generate_connections grid.connections grid.rows grid.cols grid.name

let transform_grid grid =
  match grid.rows, grid.cols, grid.name with
  | Some r, Some c, Some n ->
    let grid_objects = Utils.grid_to_strings r c n in
    NormalObjects (grid_objects)
  | _ ->
    invalid_arg "grid lacks rows/cols/name"

let transform_objects_decl obj =
  match obj with
  (* If NormalObjects do nothing *)
  | NormalObjects _ -> obj
  (* If GridAndObjects transform into NormalObjects *)
  | GridAndObjects (rows, cols, grid_name, objs) ->
    let grid_objs = Utils.grid_to_strings rows cols grid_name in
        NormalObjects (grid_objs @ objs)

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
                    OnlyArguments {a = Utils.string_of_state shape}
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
      Utils.validate_locked_matrix expected_rows expected_cols expected_name rows matrix_name
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
      let new_objects = 
        match transform_objects_decl problem_def.objects, transform_grid problem_def.grid with
        | NormalObjects objects1, NormalObjects objects2 ->
          NormalObjects (objects1 @ objects2)
        | _ ->
          failwith "Objects are not in correct format"
      in
      let grid = problem_def.grid in
      let new_init =
        match transform_grid_to_connections problem_def.grid, problem_def.init with
        | states1, states2 ->
          states1 @ states2
      in
      (*let obj_grid_data = Utils.obj_grid_data_of_objects_decl problem_def.objects in
      let new_init = transform_init obj_grid_data problem_def.init in  Make transform_init *)
      let goal = problem_def.goal in
{
  defs =
    ProblemDef {
      problem;
      problemdomain;
      objects = new_objects;
      grid;
      init = new_init;
      goal;
    }
}
