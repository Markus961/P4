open Ast

(* Count how many rows a matrix has*)
let row_count rows =
  List.fold_left (
    fun acc row ->
      match row with
      | NormalRow _ -> acc + 1
      | MultRowOption _ -> acc +1
  )
  0
  rows

(* Count how many collums a matrix has*)
let cols_count row =
  let expanded_row = Utils.expand_row row in
  List.fold_left (fun acc _ -> acc + 1) 0 expanded_row

let positive_rows_cols rows cols =
  if rows <= 0 || cols <= 0 then
    failwith "Rows and columns must be positive"

(* Validate if a shape key has already been assigned to another shape*)
let validate_unique_shapes shapes =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun s ->
      if Hashtbl.mem seen s.char_id then
        failwith ("Duplicate shape identifier in :shapes: " ^ s.char_id);
      Hashtbl.add seen s.char_id ())
    shapes

let validate_connections flags =
  List.iter (fun flag ->
    if flag <> "-H" && flag <> "-V" then
      failwith "Invalid connection flag"
  ) flags

let validate_unique_keys keys =
  let seen = Hashtbl.create 16 in
  List.iter 
    (fun key ->
      if Hashtbl.mem seen key then
        failwith ("Duplicate key-name identifier in :keys: " ^ key);
        Hashtbl.add seen key ()
  ) keys

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
  
  (* Expand rows into entries to avoid matching NormalRow and MultRowOption *)
  let entries = List.concat(Utils.expand_rows rows) in

  (* Iterate over entries *)
  List.iter (
    fun entry ->
      if entry <> "0" && entry <> "1" then
        invalid_arg (Printf.sprintf "The locked_locations matrix may only contain 0 or 1, but found '%s'" entry) 
  ) entries
    
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

  (* Expand rows into entries to avoid matching NormalRow and MultRowOption *)
  let entries = List.concat(Utils.expand_rows rows) in
  (* Iterate over entries *)
  List.iter (
    fun entry ->
      if entry <> "0" then (
        ignore (get_next_key ());
        ignore (find_shape entry))
  ) entries;

  if !remaining_keys <> [] then
    failwith "Amount of keys doesn't match with amount of keylocations in matrix"
    
let validate_locked_array expected_rows expected_cols nodes =
  List.iter
    (fun (Node (r, c)) ->
      if r < 0 || c < 0 then
        invalid_arg "Invalid node coordinates";
      if r >= expected_rows || c >= expected_cols then
        invalid_arg "Node outside grid bounds")
    nodes
   
let shape_exists shapes shape =
  List.exists (fun s -> s.shape_name = shape) shapes

(* It extracts the shape name from a state of the form:
(shape X)
- If the input matches OnlyStates with sname="shape" and exactly one argument, it returns that argument (the shape name).
- If it is an OnlyStates but not "shape", it fails with an error.
- And if it is none of the above it fails because the format is invalid.
*)  
let extract_shape_name = function
  | OnlyStates { sname = "shape"; arguments = [OnlyArguments { a }] } -> a
  | OnlyStates { sname; _ } ->
      failwith ("Expected (shape X) but got (" ^ sname ^ " ...)")
  | _ ->
      failwith "Invalid shape structure"

(* This Function collects all the above grid definition functions into one big check*)
let validate_grid grid =
  let expected_rows, expected_cols, expected_name =
  match grid.rows, grid.cols, grid.name with
  | Some r, Some c, Some n -> r, c, n
  | _ -> invalid_arg "grid lacks rows/cols/name"
  in

  (* basic grid validation *)
  validate_unique_shapes grid.shapes;
  validate_connections grid.connections;
  validate_unique_keys grid.key_names;
  positive_rows_cols expected_rows expected_cols;

  (* lockedlocations validation *)
  (match grid.locked with
  | None -> ()
  
  | Some (LockedNodesMatrix { rows = locked_rows; matrix_name; _ }) ->
    validate_locked_matrix
      expected_rows
      expected_cols
      expected_name
      locked_rows
      matrix_name

  | Some (LockedNodes (grid_name, nodes, shape)) ->
    if grid_name <> expected_name then invalid_arg "LockedNodes belongs to wrong grid";

    let shape_name = extract_shape_name shape in

    if not (shape_exists grid.shapes shape_name) then
      invalid_arg ("Unknown shape used in lockednodesarray: " ^ shape_name);

      validate_locked_array 
        expected_rows 
        expected_cols 
        nodes

  | _ -> failwith "Unknown locked nodes format"
  );

  (* keylocation validation *)
  (match grid.keyloc with
   None -> ()
     
   | Some (KeylocationMatrix { matrix_name; rows = key_rows }) ->
       validate_keylocation_matrix 
       expected_rows 
       expected_cols 
       expected_name 
       grid.key_names 
       grid.shapes 
       key_rows 
       matrix_name
       
   | _ -> failwith "Expected KeylocationMatrix in grid.keyloc"
  )

(* Checks that all matrixes defined in :grid sections are transformed properly in the :init section. *)
let rec validate_init_states grid_data states =
  let expected_rows, expected_cols, expected_name, key_names, shapes =
  match grid_data with
  | Some data -> data
  | None -> (0, 0, "", [], [])
  in

  match states with
  | [] -> ()

  | OnlyStates _ :: tl -> validate_init_states grid_data tl

  | LockedNodesMatrix { rows; matrix_name; _ } :: tl ->
      validate_locked_matrix 
      expected_rows 
      expected_cols 
      expected_name 
      rows 
      matrix_name;

      validate_init_states 
      grid_data 
      tl

  | KeylocationMatrix { rows; matrix_name; _ } :: tl ->
      validate_keylocation_matrix 
      expected_rows 
      expected_cols 
      expected_name 
      key_names 
      shapes 
      rows 
      matrix_name;
       
      validate_init_states 
      grid_data 
      tl

  | _ :: _ -> failwith "validate_init_states failure"

(*function that is sent to main. validates the entire problem *)
let validate_problem_def problem_def =
  List.iter validate_grid problem_def.gl;

  let grid_data =
    match problem_def.gl with
    | [] -> None
    | grid :: _ ->
        match grid.rows, grid.cols, grid.name with
        | Some rows, Some cols, Some name ->
            Some (rows, cols, name, grid.key_names, grid.shapes)
        | _ -> None
  in

  validate_init_states grid_data problem_def.init