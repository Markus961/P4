open Ast

(* the function takes a list of rows and a state (shape triangle) and converts it into std pddl *)
let transform_locked_matrix rows state =

  (* isolate argument from states (ex. triangle) *)
  let argument =
    match state with
      | OnlyStates { sname = "shape"; arguments = ["triangle"] } -> 
        "triangle"
      | _ -> failwith "not a shape"
      in

  (* List.flatten ensures nested lists are flattened, so only one list *)
  List.flatten (
    (* for each row i in rows *)
    List.mapi (fun i row ->
      List.flatten (
        (* for each entry j in row *)
        List.mapi (fun j entry ->
          match entry with
          | IntEntry 0 ->
              (* if entry in matrix is 0 *)
              [OnlyStates { sname = "open"; arguments = [Printf.sprintf "node%d-%d" i j] }]
          | IntEntry 1 ->
              (* if entry in matrix is 1 *)
              [ OnlyStates { sname = "locked"; arguments = [Printf.sprintf "node%d-%d" i j] };
                OnlyStates { sname = "lock-shape"; arguments = [Printf.sprintf "node%d-%d" i j; argument] } ]
          | IntEntry _ ->
              (* if entry in matrix is neither 0 or 1 but is an int *)
              failwith "Only 0 or 1 is allowed"
          | StringEntry _ ->
              (* if entry in matrix is a letter *)
              [ OnlyStates { sname = "locked"; arguments = [Printf.sprintf "node%d-%d" i j] };
                OnlyStates { sname = "lock-shape"; arguments = [Printf.sprintf "node%d-%d" i j; argument] } ]
        ) row
      )
    ) rows
  )


(*
example input:
          (:locked_nodes [
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 1 1 1 0]
            [0 0 1 1 1 0]
            [0 0 1 1 0 0]
            ] (shape triangle))

example output:
          (locked node4-3)
          (lock-shape node4-3 triangle)
          ...

          (open node0-0)
          (open node0-1)
          ...
*)