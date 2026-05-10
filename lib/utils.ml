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

(* Expands a list of MultRows (one MultRowOption) into entries*)
let expand_multrows multrows =
  List.concat (
    List.map (fun (MultRow (entries, multiplicator)) ->
      List.concat (List.init multiplicator (fun _ -> entries)) (* _ because we don't care about the index *)
    ) multrows
  )


(* Expands a row into its entries *)
let expand_row row = 
  match row with
  | NormalRow entries -> entries
  | MultRowOption multrows ->
    expand_multrows multrows 

(* List.map uses a function on all elements of a list *)
let expand_rows rows =
  List.map expand_row rows

let write_file filename content =
  let oc = open_out filename in
  output_string oc content; (* Write the entire string content into the file *)
  close_out oc