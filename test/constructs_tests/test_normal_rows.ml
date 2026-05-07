
let () = 
  print_endline "Running test on normal rows";

  let actual = Compiler_for_testing.run "normal_rows.pddl" in
  let expected = Utils.read_file "expected.pddl" in

  let trim_actual = String.trim actual in
  let trim_expected = String.trim expected in

  if trim_actual = trim_expected then
    print_endline "PASSED"
  else
    print_endline "FAILED"


