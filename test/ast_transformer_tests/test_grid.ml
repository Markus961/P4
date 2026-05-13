open Ast

let parse_program_string s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.token lexbuf

let test_grid_prog () =

  let input = 
    "(define (problem strips-grid-y-2)
        (:domain grid)
        (:objects test_object)
        (:grid 
          :rows 3
          :columns 2
          :name grid
          :connections -H
          :keys key0 key1
          :shapes ((St = star))
        )
        (:init (arm-empty)
          (at-robot grid1-0)
        )
        (:goal (at key1 grid1-1))
      )" 
  in

  let ast = parse_program_string input in

match ast.defs with
| ProblemDef { gl; _ } when gl <> [] ->
    let grid = List.hd gl in

    let count = ref [] in

    if grid.rows <> Some 3 then begin
      print_endline "Row count was parsed incorrectly";
      count := 1 :: !count
    end;

    if grid.cols <> Some 2 then begin
      print_endline "Columns count was parsed incorrectly";
      count := 1 :: !count
    end;

    if grid.name <> Some "grid" then begin
      print_endline "Name was parsed incorrectly";
      count := 1 :: !count
    end;

    if grid.connections <> ["-H"] then begin
      print_endline "Connections were parsed incorrectly";
      count := 1 :: !count
    end;

    if grid.key_names <>  ["key0"; "key1"] then begin
      print_endline "Keys were parsed incorrectly";
      count := 1 :: !count
    end;

    if grid.shapes <> [{char_id = "St"; shape_name = "star"}] then begin
      print_endline "Shape was parsed incorrectly";
      count := 1 :: !count
    end;

    if List.length !count = 0 then
      print_endline "PASSED"

| _ -> failwith "Expected grid"

let () =
  print_endline "Testing if grid-construct is parsed";
  test_grid_prog();
  