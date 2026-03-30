open Ast
let transform_locked_matrix rows state =

  let argument =
    match state with
      | OnlyStates { sname = "shape"; arguments = ["triangle"] } -> 
        "triangle"
      | _ -> failwith "not a shape"
      in

  List.flatten (
    List.mapi (fun i row ->
      (* mapper over entries i rækken *)
      List.flatten (
        List.mapi (fun j entry ->
          match entry with
          | IntEntry 0 ->
              [OnlyStates { sname = "open"; arguments = [Printf.sprintf "node%d-%d" i j] }]
          | IntEntry 1 ->
              [ OnlyStates { sname = "locked"; arguments = [Printf.sprintf "node%d-%d" i j] };
                OnlyStates { sname = "lock-shape"; arguments = [Printf.sprintf "node%d-%d" i j; argument] } ]
          | IntEntry _ ->
              failwith "Only 0 or 1 is allowed"
          | StringEntry _ ->
              [ OnlyStates { sname = "locked"; arguments = [Printf.sprintf "node%d-%d" i j] };
                OnlyStates { sname = "lock-shape"; arguments = [Printf.sprintf "node%d-%d" i j; argument] } ]
        ) row
      )
    ) rows
  )