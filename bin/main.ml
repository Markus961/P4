
let () =
  Compiler.process_file "./data/domain.pddl";
  Compiler.process_file "./data/problem_gridindefine.pddl";

  Planner.check_planner_installed ();
  Planner.run_planner ()




