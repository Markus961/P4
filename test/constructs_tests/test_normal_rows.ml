
let () = 
  print_endline "Running test on normal rows";

  let actual = Compiler_for_testing.run "normal_rows.pddl" in
  let expected = Utils.read_file "expected.pddl" in

  if actual = expected then
    print_endline "PASSED"
  else
    print_endline "FAILED"


