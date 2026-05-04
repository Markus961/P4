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

let transform_grid_to_objects grid =
  match grid.rows, grid.cols, grid.name with
  | Some r, Some c, Some n ->
    let grid_objects = Utils.grid_to_strings r c n in
    NormalObjects (grid_objects)
  | _ ->
    invalid_arg "grid lacks rows/cols/name"

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

let locked_nodes_from_grid grid = 
  match grid.locked with 
  | None -> []
  | Some (LockedNodesMatrix {rows; matrix_name; shape}) ->
    matrix_to_nodes rows matrix_name shape
  | _ -> failwith "Expected LockedNodesMatrix in grid.locked"

let transform_objects objects grid =
  let shape_names = List.map (fun s -> s.shape_name) grid.shapes in
  
  let normal_objects = 
    match objects with
    | NormalObjects obj -> obj
    | _ -> failwith "Objects are not in correct format"
  in
  
  let grid_objects =
    match transform_grid_to_objects grid with
    | NormalObjects nodes -> nodes
    | _ -> failwith "Objects are not in correct format"
  in

  NormalObjects (normal_objects @ grid_objects @ grid.key_names @ shape_names)
  

(* transform_init transforms the :init section of the PDDL problem.
It processes each state, validates special structues like LockedNodesMatrix
using grid_data, and converts them into standard PDDL predicates. *)
let rec transform_init_states grid_data states =
  match states with
  | [] -> []
  | (OnlyStates _ as s) :: tl -> 
      s :: transform_init_states grid_data tl
  | (LockedNodesMatrix { rows; matrix_name; shape }) :: tl ->
      matrix_to_nodes rows matrix_name shape @ transform_init_states grid_data tl
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

(* Converts a grid into OnlyStates for the init-section *)
let transform_init grid states =
  (* grid_data packs grid information (rows, cols, name) into an option type for later validation.
  Option type means the value may either be Some value or None if something is missing *)
  let grid_data =
    match grid.rows, grid.cols, grid.name with
    | Some r, Some c, Some n -> Some (r, c, n)
    | _ -> None
  in

  let connections = transform_grid_to_connections grid in
  let key_matrix_states = transform_keyloc grid in
  let locked_from_grid = locked_nodes_from_grid grid in

  connections @ key_matrix_states @ locked_from_grid @ transform_init_states grid_data states

let transform_program p =
  match p.defs with
  | DomainDef _ -> p
  | ProblemDef problem_def ->
      let problem = problem_def.problem in
      let problemdomain = problem_def.problemdomain in
      let grid = problem_def.grid in
      let new_objects = transform_objects problem_def.objects problem_def.grid in
      let new_init = transform_init problem_def.grid problem_def.init in
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
