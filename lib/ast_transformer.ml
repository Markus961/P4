open Ast


(* These are predicates that will be injected into the domain file, since our grid extension is dependent on these predicates*)
let inferred_predicates = [
  { pname = "conn"; variables = ["?x"; "?y"] };
  { pname = "key"; variables = ["?k"] };
  { pname = "key-shape"; variables = ["?k"; "?s"] };
  { pname = "at"; variables = ["?r"; "?x"] };
  { pname = "open"; variables = ["?x"] };
  { pname = "locked"; variables = ["?x"] };
  { pname = "lock-shape"; variables = ["?x"; "?s"] };
  { pname = "shape"; variables = ["?s"] };
  { pname = "place"; variables = ["?p"] };
  { pname = "arm_empty"; variables = [] };
  { pname = "at-robot"; variables = ["?x"] };
  { pname = "holding"; variables = ["?k"] };
  (* Add more below *)
]


(* These are actions that will be injected into the domain file, our grid extension is not dependent on these actions but it makes the extension more user-friendly *)
let move_action = {
  aname = "move";
  parameters = ["?from"; "?to"];
  precondition =
    And [
      Atom ("place", ["?from"]);
      Atom ("place", ["?to"]);
      Atom ("at-robot", ["?from"]);
      Atom ("conn", ["?from"; "?to"]);
    ];
  effects =
    And [
      Not (Atom ("at-robot", ["?from"]));
      Atom ("at-robot", ["?to"]);
    ];
}

let unlock_action = {
  aname = "unlock";
  parameters = ["?curpos"; "?lockpos"; "?key"; "?shape"];
  precondition =
    And [
      Atom ("place", ["?curpos"]);
      Atom ("place", ["?lockpos"]);
      Atom ("key", ["?key"]);
      Atom ("shape", ["?shape"]);
      Atom ("conn", ["?curpos"; "?lockpos"]);
      Atom ("key-shape", ["?key"; "?shape"]);
      Atom ("lock-shape", ["?lockpos"; "?shape"]);
      Atom ("at-robot", ["?curpos"]);
      Atom ("locked", ["?lockpos"]);
      Atom ("holding", ["?key"]);
    ];
  effects =
    And [
      Atom ("open", ["?lockpos"]);
      Not (Atom ("locked", ["?lockpos"]));
    ];
}

let pickup_action = {
  aname = "pickup";
  parameters = ["?curpos"; "?key"];
  precondition =
    And [
      Atom ("place", ["?curpos"]);
      Atom ("key", ["?key"]);
      Atom ("at-robot", ["?curpos"]);
      Atom ("at", ["?key"; "?curpos"]);
      Atom ("arm-empty", []);
    ];
  effects =
    And [
      Atom ("holding", ["?key"]);
      Not (Atom ("at", ["?key"; "?curpos"]));
      Not (Atom ("arm-empty", []));
    ];
}

let pickup_and_loose_action = {
  aname = "pickup-and-loose";
  parameters = ["?curpos"; "?newkey"; "?oldkey"];
  precondition =
    And [
      Atom ("place", ["?curpos"]);
      Atom ("key", ["?newkey"]);
      Atom ("key", ["?oldkey"]);
      Atom ("at-robot", ["?curpos"]);
      Atom ("holding", ["?oldkey"]);
      Atom ("at", ["?newkey"; "?curpos"]);
    ];
  effects =
    And [
      Atom ("holding", ["?newkey"]);
      Atom ("at", ["?oldkey"; "?curpos"]);
      Not (Atom ("holding", ["?oldkey"]));
      Not (Atom ("at", ["?newkey"; "?curpos"]));
    ];
}

let putdown_action = {
  aname = "putdown";
  parameters = ["?curpos"; "?key"];
  precondition =
    And [
      Atom ("place", ["?curpos"]);
      Atom ("key", ["?key"]);
      Atom ("at-robot", ["?curpos"]);
      Atom ("holding", ["?key"]);
    ];
  effects =
    And [
      Atom ("arm-empty", []);
      Atom ("at", ["?key"; "?curpos"]);
      Not (Atom ("holding", ["?key"]));
    ];
}

let inferred_actions = [
  move_action;
  unlock_action;
  pickup_action;
  pickup_and_loose_action;
  putdown_action;
]



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
  let expanded_rows = Utils.expand_rows rows in
  List.concat (
    List.mapi (fun i entries ->
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
    ) expanded_rows
  )

(*
    Converts a set of locked node coordinates into full PDDL state predicates for both locked and open nodes in the grid.

    The function:
    - Takes a list of locked nodes (Node (r, c)) and a shape
    - Uses the grid dimensions (rows x cols) to construct all possible nodes
    - Splits nodes into:
      - Locked nodes (given in the input)
      - Open nodes (all remianing nodes)
    
    For each locked node:
     - Generates a unique node identifier using the grid name:
        "<grid_name><row>-<col>" (e.g., "fileno3-2")
      - Produces:
        (locked <node>)
        (lock-shape <node> <shape>)

    For each open node:
      - Produces:
        (open <node>)

    The result is a flat list of all generated predicates.

*)
let generate_locked_and_open_states grid_name grid locked_nodes shape =
  match grid.rows, grid.cols with
  | Some rows, Some cols ->

      (* --- ALL NODES IN GRID --- *)
      let all_nodes =
        List.init rows (fun r ->
          List.init cols (fun c -> Node (r, c))
        )
        |> List.concat
      in

      (* --- CHECK IF NODE IS LOCKED --- *)
      let is_locked node =
        List.exists (fun n -> n = node) locked_nodes
      in

      (* --- LOCKED STATES --- *)
      let locked_states =
        List.concat (
          List.map (fun (Node (r, c)) ->
            let node_name = Printf.sprintf "%s%d-%d" grid_name r c in
            [
              OnlyStates {
                sname = "locked";
                arguments = [OnlyArguments {a = node_name}]
              };
              OnlyStates {
                sname = "lock-shape";
                arguments = [
                  OnlyArguments {a = node_name};
                  OnlyArguments {a = Utils.string_of_state shape}
                ]
              }
            ]
          ) locked_nodes
        )
      in

      (* --- OPEN STATES --- *)
      let open_states =
        List.filter (fun n -> not (is_locked n)) all_nodes
        |> List.map (fun (Node (r, c)) ->
          OnlyStates {
            sname = "open";
            arguments = [
              OnlyArguments {
                a = Printf.sprintf "%s%d-%d" grid_name r c
              }
            ]
          }
        )
      in

      locked_states @ open_states

  | _ -> failwith "Grid missing rows/cols"


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
        let expanded_rows = Utils.expand_rows rows in
          List.concat (
            List.mapi (fun i entries ->
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
            ) expanded_rows
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

  | Some (LockedNodes (grid_name, nodes, shape)) ->
      (match grid.name with
       | Some name when name = grid_name ->
           generate_locked_and_open_states name grid nodes shape
       | Some name ->
           failwith ("LockedNodesArray belongs to grid " ^ grid_name ^ " but current grid is " ^ name)
       | None ->
           failwith "Grid has no name defined")
  
  
  | _ -> failwith "unexpected locked node format"

let transform_objects objects grid_opt =
  match grid_opt with
    | Some grid -> (*if there are a grid*)
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
    | None ->  (*if there are no grid*)
        objects
  

(* transform_init transforms the :init section of the PDDL problem.
It processes each state, validates special structues like LockedNodesMatrix
using grid_data, and converts them into standard PDDL predicates. *)
let rec transform_init_states grid_data states =
  match states with
  | [] -> []
  | (OnlyStates _ as s) :: tl -> 
      s :: transform_init_states grid_data tl
  | (LockedNodesMatrix _) :: tl ->
    transform_init_states grid_data tl
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
let transform_init grid_opt states =
  match grid_opt with
    | Some grid ->
      (* grid_data packs grid information (rows, cols, name) into an option type for later validation.
      Option type means the value may either be Some value or None if something is missing *)
      let grid_data =
        match grid.rows, grid.cols, grid.name with
        | Some r, Some c, Some n -> Some (r, c, n)
        | _ -> None
      in

  (*Adds place to each node in the grid*)
  let place_states =
    match grid.rows, grid.cols, grid.name with
    | Some r, Some c, Some n ->
        List.map
          (fun node_name ->
            OnlyStates {
              sname = "place";
              arguments = [OnlyArguments { a = node_name }];
            })
          (Utils.grid_to_strings r c n)
    | _ -> []
  in


      let connections = transform_grid_to_connections grid in
      let key_matrix_states = transform_keyloc grid in
      let locked_from_grid = locked_nodes_from_grid grid in

     place_states @ connections @ key_matrix_states @ locked_from_grid @ transform_init_states grid_data states
    | None ->
        List.filter (function
        | OnlyStates _ -> true (*only keep states which are onlystates*)
        | _ -> false (*things we dont want could be: LockedNodesMatrix, KeylocationMatrix, OpenNodes. There should be in grid section and not directly in init*)
      ) states

let transform_program p =
  match p.defs with
  | DomainDef d -> 
      let new_domain = { d with
        predicates = inferred_predicates @ d.predicates;
        actions = inferred_actions @ d.actions; } in
      { defs = DomainDef new_domain }

  | ProblemDef problem_def ->
      let problem = problem_def.problem in
      let problemdomain = problem_def.problemdomain in
      let grids = problem_def.gl in
      let new_objects =
        List.fold_left
          (fun objs grid -> transform_objects objs (Some grid))
          problem_def.objects
          grids
      in
      let new_init =
        List.fold_left
          (fun init grid -> transform_init (Some grid) init)
          problem_def.init
          grids
      in
      let goal = problem_def.goal in

  {
    defs =
      ProblemDef {  
        problem;
        problemdomain;
        objects = new_objects;
        gl = grids;
        init = new_init;
        goal;
      }
  }
