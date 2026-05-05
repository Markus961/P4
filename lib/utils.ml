open Ast
let generate_horizontal grid_name rows cols =
  let acc = ref [] in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 2 do
      let a = Printf.sprintf "%s%d-%d" grid_name i j in
      let b = Printf.sprintf "%s%d-%d" grid_name i (j+1) in
      
      (*  Each pair is added in both directions so connections are bidirectional. *)
      (* A -> B *)
      acc := OnlyStates {
        sname = "conn";
        arguments = [
          OnlyArguments {a = a};
          OnlyArguments {a = b};
        ];
      } :: !acc;

      (* B -> A *)
      acc := OnlyStates {
        sname = "conn";
        arguments = [
          OnlyArguments {a = b};
          OnlyArguments {a = a};
        ];
      } :: !acc;

    done
  done;
  List.rev !acc

let generate_vertical grid_name rows cols =
  let acc = ref [] in
  for i = 0 to rows - 2 do
    for j = 0 to cols - 1 do
      let a = Printf.sprintf "%s%d-%d" grid_name i j in
      let b = Printf.sprintf "%s%d-%d" grid_name (i+1) j in

      (*  Each pair is added in both directions so connections are bidirectional. *)
      (* A -> B *)
      acc := OnlyStates {
        sname = "conn";
        arguments = [
          OnlyArguments {a = a};
          OnlyArguments {a = b};
        ];
      } :: !acc;

      (* B -> A *)
      acc := OnlyStates {
        sname = "conn";
        arguments = [
          OnlyArguments {a = b};
          OnlyArguments {a = a};
        ];
      } :: !acc;

    done
  done;
  List.rev !acc

let grid_to_strings rows cols grid_name =
  let acc = ref [] in
  if rows <= 0 || cols <= 0 then
    invalid_arg "row and cols must be positive";
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      acc := Printf.sprintf "%s%d-%d" grid_name i j :: !acc
    done;
  done;
  (* List is built in reverse so it must be reversed *)
  List.rev !acc

let rec string_of_arguments args = 
  match args with
  | [] -> []
  | OnlyArguments { a } :: tl ->
      a :: string_of_arguments tl
  | _ -> []

let string_of_state = function
  | OnlyStates { sname = _ ; arguments } -> 
    let args = string_of_arguments arguments in
      String.concat "" args
  | LockedNodesMatrix _ -> ""
  | LockedNodes _ ->  ""
  | OpenNodes _ -> ""
  | KeylocationMatrix _ -> ""
  | GridConnection _ -> ""