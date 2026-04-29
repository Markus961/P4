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

(* Unused *)
(* takes gridAndObjects and put the variables into Some(or None if there is no grid) *)
let obj_grid_data_of_objects_decl = function
  | GridAndObjects (rows, cols, grid_name, _) -> Some (rows, cols, grid_name)
  | NormalObjects _ -> None