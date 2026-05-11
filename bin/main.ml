<<<<<<< comments

let () = Compiler.run "./test/constructs_tests/mult_rows.pddl"
=======

let () =
  Compiler.process_file "./data/domain.pddl";
  Compiler.process_file "./data/problem_grid.pddl";

  Planner.check_planner_installed ();
  Planner.run_planner ()
>>>>>>> main




