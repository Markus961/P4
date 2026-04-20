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

let expand_row = function
    | RowNormal (NormalRow entries) -> entries
    | RowRepeat row_part_list ->
      (* Convert something like [1]*3 + [0]*2 into a flat list: [1;1;1;0;0] *)
       List.concat (
         List.map (fun (MultRow(entries, n)) ->
           List.concat (
             (* Repeat entries n times: [[entries];[entries];...;[entries]] *)
             List.init n (fun _ -> entries)) 
          ) row_part_list
        )

let matrix_to_nodes row_expr_list matrix_name shape =
  List.concat (
    List.mapi (fun i row_expr ->

      (* 1. Normalize row -> flat entry list *)
      let entries =
        match row_expr with
        | RowNormal (NormalRow entries) -> entries
        | RowRepeat _ -> expand_row row_expr
      in

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
                      [
                        OnlyStates {
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
    ) row_expr_list
  )


let rec transform_init (states : state list) =
  match states with
  | [] -> []
  | (OnlyStates _ as s) :: tl -> 
      s :: transform_init tl
  | (LockedNodesMatrix { rows; matrix_name; shape }) :: tl ->
      matrix_to_nodes rows matrix_name shape @ transform_init tl
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
