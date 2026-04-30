open Ast

(* Count how many rows a matrix has*)
let row_count rows =
  List.fold_left (
    fun acc row ->
      match row with
      | NormalRow _ -> acc + 1
      | MultRow _ -> acc +1
  )
  0
  rows

    (* Count how many collums a matrix has*)
let cols_count = function
  | NormalRow entries -> List.length entries
  | MultRow (_, num_of_entries) -> 
    print_endline "HUSK fix så man kan have liste af multrow"; num_of_entries

  (* Validate if a shape key has already been assigned to another shape*)
let validate_unique_shapes shapes =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun s ->
      if Hashtbl.mem seen s.char_id then
        failwith ("Duplicate shape identifier in :shapes: " ^ s.char_id);
      Hashtbl.add seen s.char_id ())
    shapes

  (*  matrix validator 'template' for both locked and keylocation matrices. *)
let validate_matrix_basic expected_rows expected_cols expected_name rows matrix_name =
  if matrix_name <> expected_name then
    invalid_arg
      (Printf.sprintf
         "matrix uses '%s' but objects grid name is '%s'"
         matrix_name
         expected_name);
  let actual_rows = row_count rows in
  if actual_rows <> expected_rows then
    invalid_arg
      (Printf.sprintf
         "matrix has %d rows but objects grid expects %d"
         actual_rows
         expected_rows);
  List.iter
    (fun row ->
      let actual_cols = cols_count row in
      if actual_cols <> expected_cols then
        invalid_arg
          (Printf.sprintf
             "matrix row has %d cols but objects grid expects %d"
             actual_cols
             expected_cols))
    rows
        (*Validate locked matrix using the function above. check entries. only 0 and 1's allowed *)
let validate_locked_matrix expected_rows expected_cols expected_name rows matrix_name =
  validate_matrix_basic expected_rows expected_cols expected_name rows matrix_name;
  List.iter
    (fun row ->
      match row with
      | NormalRow entries ->
          List.iter
            (fun entry ->
              if entry <> "0" && entry <> "1" then
                invalid_arg
                  (Printf.sprintf
                     "locked_nodes_matrix may only contain 0 or 1, but found '%s'"
                     entry)
            )
            entries
      | MultRow (entries, _) -> 
          List.iter
              (fun entry ->
                if entry <> "0" && entry <> "1" then
                  invalid_arg (
                    Printf.sprintf "locked_nodes_matrix may only contain 0 or 1, but found '%s'" entry
                  )
              ) entries
    ) rows
    
        (*Validate key matrix using the function 2 times above. check keys *)
let validate_keylocation_matrix expected_rows expected_cols expected_name key_names shapes rows matrix_name =
  validate_matrix_basic expected_rows expected_cols expected_name rows matrix_name;
  let remaining_keys = ref key_names in
  let get_next_key () =
    match !remaining_keys with
    | [] -> failwith "More symbols in the matrix than keys in :keys"
    | hd :: tl ->
        remaining_keys := tl;
        hd
  in
  let find_shape char_id =
    match List.find_opt (fun s -> s.char_id = char_id) shapes with
    | Some s -> s.shape_name
    | None -> failwith ("Unknown symbol in the matrix: "  ^ char_id)
  in
  List.iter
    (fun row ->
      match row with
      | NormalRow entries ->
          List.iter
            (fun entry ->
              if entry <> "0" then (
                ignore (get_next_key ());
                ignore (find_shape entry)))
            entries
      | MultRow _ -> failwith "MultRow is not supported by :keylocations yet")
    rows;
  if !remaining_keys <> [] then
    failwith "Amount of keys doesn't match with amount of keylocations in matrix"
    
    (*Tnis Function collects all the above grid definition functions into one big check*)
let validate_grid grid =
  match grid.rows, grid.cols, grid.name with
  | Some expected_rows, Some expected_cols, Some expected_name ->
      validate_unique_shapes grid.shapes;
      (match grid.locked with
       | None -> ()
     | Some (LockedNodesMatrix { rows = locked_rows; matrix_name; _ }) ->
       validate_locked_matrix expected_rows expected_cols expected_name locked_rows matrix_name
       | _ -> failwith "Expected LockedNodesMatrix in grid.locked");
      (match grid.keyloc with
       | None -> ()
     | Some (KeylocationMatrix { matrix_name; rows = key_rows }) ->
       validate_keylocation_matrix expected_rows expected_cols expected_name grid.key_names grid.shapes key_rows matrix_name
       | _ -> failwith "Expected KeylocationMatrix in grid.keyloc")
  | _ -> invalid_arg "grid lacks rows/cols/name"
        (* checks that all matrixes in init fits with locked + keylocation grids *)
let rec validate_init_states grid_data states =
  match states with
  | [] -> ()
  | OnlyStates _ :: tl -> validate_init_states grid_data tl
  | LockedNodesMatrix { rows; matrix_name; _ } :: tl ->
      (match grid_data with
       | Some (expected_rows, expected_cols, expected_name, _, _) ->
           validate_locked_matrix expected_rows expected_cols expected_name rows matrix_name
       | None -> invalid_arg "locked_nodes_matrix doesn't match :objects (:grid ...)");
      validate_init_states grid_data tl
  | KeylocationMatrix { rows; matrix_name; _ } :: tl ->
      (match grid_data with
       | Some (expected_rows, expected_cols, expected_name, key_names, shapes) ->
           validate_keylocation_matrix expected_rows expected_cols expected_name key_names shapes rows matrix_name
       | None -> invalid_arg "keylocation_matrix doesn't match :objects (:grid ...)");
      validate_init_states grid_data tl
  | _ :: _ -> failwith "validate_init_states failure"

  (*function that is sent to main. validates the entire problem *)
let validate_problem_def problem_def =
  validate_grid problem_def.grid;
  let grid_data =
    match problem_def.grid.rows, problem_def.grid.cols, problem_def.grid.name with
    | Some rows, Some cols, Some name -> Some (rows, cols, name, problem_def.grid.key_names, problem_def.grid.shapes)
    | _ -> None
  in
  validate_init_states grid_data problem_def.init