
let expand_grid rows cols extra =
  let nodes =
    List.init rows (fun r ->
      List.init cols (fun c -> Printf.sprintf "node%d-%d" r c)
    )
    |> List.flatten
  in
  nodes @ extra