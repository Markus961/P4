open Ast

let test_generate_vertical () =
  print_endline "Running test on generate_vertical";
  let result = Ast_transformer.generate_vertical "node" 3 2 in
  let expected = [
    OnlyStates 
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node0-0"}; 
        OnlyArguments {a = "node1-0"}]};

    OnlyStates
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node0-1"}; 
        OnlyArguments {a = "node1-1"}]}; 
    OnlyStates 
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node1-0"}; 
        OnlyArguments {a = "node2-0"}]};
    OnlyStates 
      {sname = "conn"; arguments = [
        OnlyArguments {a = "node1-1"}; 
        OnlyArguments {a = "node2-1"}]};
  ]
  in

  assert (result = expected);
  print_endline "PASSED"

let () =
  test_generate_vertical ()