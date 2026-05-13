
let () =
  Compiler.process_file "./data/domain.pddl";
<<<<<<< Inferred-Predicates+Readme
  Compiler.process_file "./data/problem_grid.pddl";
=======
  Compiler.process_file "./data/problem_gridindefine.pddl";
>>>>>>> main

  Planner.check_planner_installed ();
  Planner.run_planner ()




