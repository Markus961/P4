(* open Ast

(* :grid transformer: skal ikke printe nodes men lave GridAndObjects om til NormalObjects *)
let expand_grid rows cols extra =
  let nodes =
    List.init rows (fun r ->
      List.init cols (fun c -> Printf.sprintf "node%d-%d" r c)
    )
    |> List.flatten
  in
  nodes @ extra



  (* locked_nodes_matrix transformer *)
  let locked_nodes_matrix_transformer rows state = 
    assert false

  *)



  (* i main: i main: 
  let parsed = Parser.problem Lexer.token lexbuf in 
  let transformed = Transform.transform_program parsed in transformed *)