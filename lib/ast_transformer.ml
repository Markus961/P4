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
  { pname = "arm-empty"; variables = [] };
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

let transform_grid_to_places grid =
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

let extract_shape_names grid =
  List.map (fun s -> s.shape_name) grid.shapes

let extract_normal_objects objects =
  match objects with
    | NormalObjects obj -> obj
    | _ -> failwith "Objects are not in correct format"

let extract_grid_objects grid =
  match transform_grid_to_objects grid with
    | NormalObjects nodes -> nodes
    | _ -> failwith "Objects are not in correct format"

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

let generate_locked_states locked_nodes grid_name shape =
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

let generate_open_states locked_nodes all_nodes grid_name =
  List.filter (fun n -> not (Utils.is_locked n locked_nodes)) all_nodes
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

let generate_locked_and_open_states grid_name grid locked_nodes shape =
  match grid.rows, grid.cols with
  | Some rows, Some cols ->

      let all_nodes = Utils.get_all_nodes rows cols in
      let locked_states = generate_locked_states locked_nodes grid_name shape in
      let open_states = generate_open_states locked_nodes all_nodes grid_name in

      locked_states @ open_states

  | _ -> failwith "Grid missing rows/cols"

let get_next_key remaining_keys =
  match !remaining_keys with
  | [] -> failwith "More symbols in the matrix than keys in :keys"
  | hd :: tl -> 
    remaining_keys := tl; 
    hd

let find_shape shapes char_id =
  match List.find_opt (fun s -> s.char_id = char_id) shapes with
  | Some s -> s.shape_name 
  | None -> failwith ("Unknown symbol in the matrix: " ^ char_id)

let make_keylocation_states key_name shape_name node_name =
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

let transform_keylocation_entries remaining_keys shapes matrix_name rows =
  List.concat (
    List.mapi (fun i entries ->
      List.concat (
        List.mapi (fun j entry ->
          if entry = "0" then []
          else
            let key_name = get_next_key remaining_keys in
            let shape_name = find_shape shapes entry in
            let node_name = Printf.sprintf "%s%d-%d" matrix_name i j in

            make_keylocation_states key_name shape_name node_name
                
        ) entries
      )
    ) rows
  )
                      

let transform_keyloc grid =
  let remaining_keys = ref grid.key_names in
 
  match grid.keyloc with
  | Some (KeylocationMatrix { matrix_name; rows }) ->
    
    let expanded_rows = Utils.expand_rows rows in
    let keylocation_states = transform_keylocation_entries remaining_keys grid.shapes matrix_name expanded_rows in

    if !remaining_keys <> [] then failwith "More keys in :keys than symbols in :keylocations";

    keylocation_states

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
    | Some grid ->
        let shape_names = extract_shape_names grid in
        let normal_objects = extract_normal_objects objects in
        let grid_objects = extract_grid_objects grid in

        NormalObjects (normal_objects @ grid_objects @ grid.key_names @ shape_names)

    | None ->
        objects
  
let only_states states =
  List.filter (function OnlyStates _ -> true | _ -> false) states

let transform_init grid_opt states =
  match grid_opt with
  | Some grid ->
      let place_states = transform_grid_to_places grid in
      let connections = transform_grid_to_connections grid in
      let key_matrix_states = transform_keyloc grid in
      let locked_from_grid = locked_nodes_from_grid grid in

      place_states @ connections @ key_matrix_states @ locked_from_grid @ only_states states

  | None ->
      only_states states

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
        gl = [];
        init = new_init;
        goal;
      }
  }
