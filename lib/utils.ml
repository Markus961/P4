open Ast
let generate_horizontal grid_name rows cols =
  let acc = ref [] in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 2 do

        let arg_list = ref [] in
        arg_list := OnlyArguments {a = Printf.sprintf "%s%d-%d" grid_name i (j+1)} :: !arg_list;
        arg_list := OnlyArguments {a = Printf.sprintf "%s%d-%d" grid_name i j} :: !arg_list;
        
        acc := OnlyStates {sname = "conn"; arguments = !arg_list} :: !acc
        
    done
  done;
  List.rev !acc

let generate_vertical grid_name rows cols =
  let acc = ref [] in
  for i = 0 to rows - 2 do
    for j = 0 to cols - 1 do
      
      let arg_list = ref [] in
      arg_list := OnlyArguments {a = Printf.sprintf "%s%d-%d" grid_name (i+1) j} :: !arg_list;
      arg_list := OnlyArguments {a = Printf.sprintf "%s%d-%d" grid_name i j} :: !arg_list;
        
      acc := OnlyStates {sname = "conn"; arguments = !arg_list} :: !acc
        
    done
  done;
  List.rev !acc

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

let row_count rows =
  List.fold_left
    (fun acc row ->
      match row with
      | NormalRow _ -> acc + 1
      | MultRow _ ->
        failwith (Printf.sprintf "error"))
    0
    rows

let cols_count = function
  | NormalRow entries -> List.length entries
  | MultRow _ ->
      failwith (Printf.sprintf "error")

let validate_unique_shapes shapes =
  let seen = Hashtbl.create 16 in
  List.iter (fun s ->
    if Hashtbl.mem seen s.char_id then
      failwith
        ("Duplicate shape identifier in :shapes: " ^ s.char_id);
    Hashtbl.add seen s.char_id ()
  ) shapes  

let validate_matrix_name grid_name matrix_name =
  if grid_name <> matrix_name then
    failwith
      (Printf.sprintf
        "Matrix name '%s' does not match grid name '%s'" matrix_name grid_name)

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
  | KeylocationMatrix _ -> ""
  | GridConnection _ -> ""

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