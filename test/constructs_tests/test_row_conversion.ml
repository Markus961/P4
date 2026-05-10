
let () = 
  print_endline "Running test on normal rows";

  let actual_normal = Compiler_for_testing.run "normal_rows.pddl" in
  let expected_normal = Utils.read_file "expected_normal.pddl" in

  let trim_actual_normal = String.trim actual_normal in
  let trim_expected_normal = String.trim expected_normal in

  if trim_actual_normal = trim_expected_normal then
    print_endline "PASSED"
  else
    print_endline "FAILED";



  print_endline "Running test on mult rows";

  let actual_mult = Compiler_for_testing.run "mult_rows.pddl" in
  let expected_mult = Utils.read_file "expected_mult.pddl" in

  let trim_actual_mult = String.trim actual_mult in
  let trim_expected_mult = String.trim expected_mult in

  if trim_actual_mult = trim_expected_mult then
    print_endline "PASSED"
  else
    print_endline "FAILED"


