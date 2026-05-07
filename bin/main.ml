
let () = Compiler.run "./test/constructs_tests/normal_rows.pddl"

  (* before code used to print to terminal *)
  (* 
  match transformed_ast.defs with
    | DomainDef _ ->
      print_endline "DOMAIN"; (* Remove from final program *)
      Pddl_printer.print_program transformed_ast
    | ProblemDef _ ->
      print_endline "PROBLEM";
      Pddl_printer.print_program transformed_ast
  *)



