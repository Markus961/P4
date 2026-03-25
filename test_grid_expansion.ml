(* test_objects.ml *)
open Ast  (* antager ast.ml indeholder odef, objects_decl, expand_grid, objects_decl_to_odef *)
open Expand_grid

let () =
  (* Test GridAndObjects *)
  let test_decl = GridAndObjects (6, 6, ["triangle"; "diamond"; "key0"]) in

  (* Oversæt til odef list *)
  let all_odefs = objects_decl_to_odef test_decl in

  (* Print resultatet *)
  print_endline "Expanded objects from grid + extra objects:";
  List.iter (fun o -> print_endline o.oname) all_odefs