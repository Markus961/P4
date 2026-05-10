open Ast
let test_generate_horizontal () =
  print_endline "Running test on generate_horizontal";
  let result = Utils.generate_horizontal "node" 2 2 in
  let expected = [
    OnlyStates 
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node0-0"}; 
        OnlyArguments {a = "node0-1"}]};
    OnlyStates 
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node0-1"}; 
        OnlyArguments {a = "node0-0"}]};
    OnlyStates
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node1-0"}; 
        OnlyArguments {a = "node1-1"}]};
    OnlyStates
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node1-1"}; 
        OnlyArguments {a = "node1-0"}]};
    
  ]
  in

  if result = expected then
    print_endline "PASSED"
  else
    print_endline "FAILED"

let () =
  test_generate_horizontal ()