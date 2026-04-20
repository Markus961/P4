open Ast

let grid_to_strings rows cols grid_name =
  let acc = ref [] in
    for i = 0 to rows - 1 do
      for j = 0 to cols -1 do
        acc := Printf.sprintf "%s%d-%d" grid_name i j :: !acc
      done
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
            | '0' ->
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

let matrix_to_key_states rows matrix_name all_init_states =
  let shapes = List.filter_map (function
    | OnlyStates { sname = "shape"; arguments = [OnlyArguments {a = shape_name}] } -> Some shape_name
    | _ -> None
  ) all_init_states in 
  
  let keys = List.filter_map (function
  
    | OnlyStates { sname = "key"; arguments = [OnlyArguments {a = key_name}] } -> Some key_name
    | _ -> None
  ) all_init_states in

  let available_keys = ref keys in

  List.concat (List.mapi (fun i row ->
    match row with
    
    | NormalRow entries ->
        List.fold_left (fun acc (j, entry) ->
          if entry = '-' then acc
          else
            
            let symbol_lower = Char.lowercase_ascii entry in
            let found_shape = List.find_opt (fun s ->
              String.length s > 0 && Char.lowercase_ascii s.[0] = symbol_lower) shapes in
              
              match found_shape with
              | None -> acc
              | Some s_name ->
                  match !available_keys with
                  
                  | [] -> failwith "There are more symbols in the matrix then the defined keys"
                  | current_key :: tail ->
                      available_keys := tail;
                      let node_name = Printf.sprintf "%s%d-%d" matrix_name i j in
                      
                      
                      let new_states = [
                        OnlyStates { sname = "key-shape"; arguments = [OnlyArguments {a = current_key}; OnlyArguments {a = s_name}] };
                        OnlyStates { sname = "at"; arguments = [OnlyArguments {a = current_key}; OnlyArguments {a = node_name}] }
                      ] in
                      acc @ new_states
        ) [] (List.mapi (fun j e -> (j, e)) entries)
    | _ -> []
  ) rows)

let rec transform_init_helper all_states (states_to_process : state list) =
  match states_to_process with
  | [] -> []
  | (OnlyStates _ as s) :: tl -> 
      s :: transform_init_helper all_states tl
  | (LockedNodesMatrix { rows; matrix_name; shape }) :: tl ->
      matrix_to_nodes rows matrix_name shape @ transform_init_helper all_states tl
  |  (KeylocationMatrix { rows; matrix_name }) :: tl -> matrix_to_key_states rows matrix_name all_states @ transform_init_helper all_states tl   
  (* | LockedNodes (nodes, st) as hd :: tl ->
      hd :: transform_init tl
  | OpenNodes (rc, st) as hd :: tl ->
      hd :: transform_init tl
  | Keys keys as hd :: tl ->
      hd :: transform_init tl
  | GridConnection flags as hd :: tl ->
      hd :: transform_init tl *)
  | _ ->
      failwith ("Hi failure")

let transform_init states =
  transform_init_helper states states      

let transform_program p =
  match p.defs with
  (* We didn't provide extensions for domain.pddl so nothing happens *)
  | DomainDef _ -> p
  | ProblemDef problem_def ->
      let problem = problem_def.problem in
      let problemdomain = problem_def.problemdomain in
      let new_objects = transform_objects_decl problem_def.objects in
      let new_init = transform_init problem_def.init in (* Make transform_init *)
      let goal = problem_def.goal in
{
  defs =
    ProblemDef {
      problem;
      problemdomain;
      objects = new_objects;
      init = new_init;
      goal;
    }
}
