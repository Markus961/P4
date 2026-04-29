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
let transform_grid grid =
  match grid.rows, grid.cols, grid.name with
  | Some r, Some c, Some n ->
    let grid_objects = grid_to_strings r c n in
    NormalObjects (grid_objects)
  | _ ->
    invalid_arg "grid lacks rows/cols/name"

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

  let validate_matrix_name grid_name matrix_name =
  if grid_name <> matrix_name then
    failwith
      (Printf.sprintf
         "Matrix name '%s' does not match grid name '%s'"
         matrix_name grid_name)

let transform_keyloc (grid : Ast.grid) =
  let remaining_keys = ref grid.key_names in
  
  let get_next_key () =
    match !remaining_keys with

    | [] -> failwith "More symbols in the matrix than keys in :keys"
    | hd :: tl -> remaining_keys := tl; hd
  in

  let find_shape char_id =
    match List.find_opt (fun s -> s.char_id = char_id) grid.shapes with

    | Some s -> s.shape_name
    | None -> failwith ("Unknown symbol in the matrix: " ^ char_id)
  in

  match grid.keyloc with

  | Some (KeylocationMatrix { matrix_name; rows }) ->
      (match grid.name with
     | Some grid_name ->
         validate_matrix_name grid_name matrix_name
     | None ->
         failwith "Grid has no name");

      let result =
        List.concat (
          List.mapi (fun i row ->
            match row with
            | NormalRow entries ->
                List.concat (
                  List.mapi (fun j entry ->
                    if entry = "0" then
                      []
                    else
                      let key_name = get_next_key () in
                      let shape_name = find_shape entry in
                      let node_name =
                        Printf.sprintf "%s%d-%d" matrix_name i j
                      in
                      [
                        OnlyStates {
                          sname = "key";
                          arguments = [
                            OnlyArguments { a = key_name }
                          ];
                        };

                        OnlyStates {
                          sname = "key-shape";
                          arguments = [
                            OnlyArguments { a = key_name };
                            OnlyArguments { a = shape_name };
                          ];
                        };

                        OnlyStates {
                          sname = "at";
                          arguments = [
                            OnlyArguments { a = key_name };
                            OnlyArguments { a = node_name };
                          ];
                        };
                      ]
                  ) entries
                )

            | MultRow _ ->
                failwith
                  "MultRow is not supported by :keylocations yet"
          ) rows
        )
      in

      if !remaining_keys <> [] then
        failwith "More keys in :keys than symbols in :keylocations";

      result

  | _ -> []

let validate_unique_shapes shapes =
  let seen = Hashtbl.create 16 in
  List.iter (fun s ->
    if Hashtbl.mem seen s.char_id then
      failwith
        ("Duplicate shape identifier in :shapes: " ^ s.char_id);
    Hashtbl.add seen s.char_id ()
  ) shapes  


let rec transform_init grid_data (states : state list) =
  match states with
  | [] -> []
  | (OnlyStates _ as s) :: tl -> 
      s :: transform_init grid_data tl
  | (LockedNodesMatrix { rows; matrix_name; shape }) :: tl ->
  (match grid_data with
    | Some (expected_rows, expected_cols, expected_name) -> (*compare lockedNodeMatrix with gridobj. *)
      validate_locked_matrix expected_rows expected_cols expected_name rows matrix_name
    | None ->
      invalid_arg "locked_nodes_matrix doesn't match :objects (:grid ...)");  
      matrix_to_nodes rows matrix_name shape @ transform_init grid_data tl
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
      let grid = problem_def.grid in
      validate_unique_shapes grid.shapes;

      (* new_objects translate the :objects section for the PDDL problem it combines grid defined objects with regular object
        like key objects from :keys and shape objects from :shapes the result is a long list of objects needed in the domain *)
      let new_objects = 
        match transform_objects_decl problem_def.objects, transform_grid grid with
        | NormalObjects obj1, NormalObjects nodes ->
          let shape_names = List.map (fun s -> s.shape_name) grid.shapes in
          NormalObjects (obj1 @ nodes @ grid.key_names @ shape_names)
        | _ ->
          failwith "Objects are not in correct format"
      in

      (* grid_data packs grid information (rows, cols, name) into an option type for later validation.
      Option type means the value may either be Some value or None if something is missing *)
      let grid_data = match grid.rows, grid.cols, grid.name with
        | Some r, Some c, Some n -> Some (r, c, n)
        | _ -> None
      in


      let key_matrix_states = transform_keyloc grid in

      let new_init =  
        key_matrix_states @ 
        transform_init grid_data problem_def.init 
      in

       (* Make transform_init *)
      (*let obj_grid_data = obj_grid_data_of_objects_decl problem_def.objects in
      let new_init = transform_init obj_grid_data problem_def.init in  Make transform_init *)
    
{
  defs =
    ProblemDef {
      problem = problem;
      problemdomain = problemdomain;
      objects = new_objects;
      grid = grid;
      init = new_init;
      goal = problem_def.goal;
    }
}
