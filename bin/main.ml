
let () =
  Compiler.process_file "./data/domain.pddl";
  Compiler.process_file "./data/repeatmultoption.pddl";

  Planner.check_planner_installed ();
  Planner.run_planner ()




