open Ast

(* helper function - Writes a string to a file *)
let write_file filename content =
  let oc = open_out filename in
  output_string oc content; (* Write the entire string content into the file *)
  close_out oc

(* Read one input file, transform it, and write the matching output file. *)
let process_file input_path =
  print_endline "START"; (* Debug: did the function start *)
  let input_channel = open_in input_path in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in input_channel;

  (* Only problem files need extra validation before transformation. *)
  (match ast.defs with
  | DomainDef _ -> ()
  | ProblemDef problem_def ->
      Ast_validator.validate_problem_def problem_def); (*Call the Ast_validator to check the input*)

  (*Transform*)
  let transformed_ast = Ast_transformer.transform_program ast in

  (* Debug print - code for writing to terminal (optional) *)
  let debug = true in
  if debug then (
    print_endline "=== DEBUG OUTPUT ===";
    Pddl_printer.print_program transformed_ast
  );

  (* Convert the transformed AST to PDDL text and write it to disk. *)
  match transformed_ast.defs with
  | DomainDef d ->
    print_endline "DEBUG DOMAIN REACHED";

    print_endline "Generating DOMAIN file...";

    let content = Domain_generator.string_of_domain d in
    write_file "transformed_domain.pddl" content;

    print_endline "Wrote transformed_domain.pddl"

  | ProblemDef p ->
    print_endline "Generating PROBLEM file...";

    let content = Problem_generator.string_of_problem p in
    write_file "transformed_problem.pddl" content;

    print_endline "Wrote transformed_problem.pddl"

    
let planner_path = ref None

let find_home_dir () =
  (* Try the standard home variable first. *)
  match Sys.getenv_opt "HOME" with
  | Some home -> Some home
  (* On Windows, use the user profile path instead. *)
  | None -> Sys.getenv_opt "USERPROFILE"

let check_planner_installed () =
  (* Allow the planner path to be set explicitly on any system. *)
  match Sys.getenv_opt "FAST_DOWNWARD_PATH" with
  | Some custom_path when Sys.file_exists custom_path ->
      (* Store the custom path so run_planner can use it later. *)
      planner_path := Some custom_path
  | Some custom_path ->
      (* Stop early if the custom path does not exist. *)
      print_endline ("\nERROR: FAST_DOWNWARD_PATH not found: " ^ custom_path);
      exit 1
  | None ->
      (* Fallback path works on macOS/Linux (HOME) and Windows (USERPROFILE). *)
      (match find_home_dir () with
      | None ->
          (* We cannot build a path if no home directory is available. *)
          print_endline "Could not determine HOME/USERPROFILE directory.";
          exit 1
      | Some home ->
          (* Build the default Fast Downward path inside the home folder. *)
          let path = Filename.concat home "planners/downward/fast-downward.py" in
          if Sys.file_exists path then
            (* Save the path when the planner exists at the default location. *)
            planner_path := Some path
          else (
            (* Tell the user exactly where we expected the planner. *)
            print_endline "\nERROR: Fast Downward not found.";
            print_endline ("Expected at: " ^ path);
            print_endline "Set FAST_DOWNWARD_PATH or see PLANNER_SETUP.md.";
            exit 1
          ))

(* Run Fast Downward automatically after transformation *)
let run_planner () =
  (* Run the planner on the generated domain/problem files. *)
  (* Save the output and also print it to the terminal. *)
  match !planner_path with
  | None ->
      (* check_planner_installed was not called, or it failed. *)
      print_endline "Planner path not set.";
      exit 1
  | Some path ->
      (* This message tells the user that planning has started. *)
      print_endline "\nRunning Fast Downward...";

      (* File where we store the full planner log. *)
      let output_filename = "detailed_solution_plan.txt" in

      (* Build the command that runs Fast Downward on the translated PDDL files. *)
      let planner_exec =
        (* Windows needs an explicit python call for the .py script. *)
        if Sys.os_type = "Win32" then
          "python \"" ^ path ^ "\""
        (* macOS/Linux can run the script directly. *)
        else
          "\"" ^ path ^ "\""
      in
      (* Add the translated domain, problem, and search strategy. *)
      let cmd =
        planner_exec ^
        " transformed_domain.pddl transformed_problem.pddl \
         --search \"astar(lmcut())\""
      in

      (* Unix is OCaml's standard module for operating-system features like processes. *)
      (* open_process_in starts the command and gives us a channel for reading stdout. *)
      let ic = Unix.open_process_in cmd in
      (* Write the same planner output to a file for later inspection. *)
      let oc = open_out output_filename in

      (* Read planner output line by line until the process ends. *)
      (try
         while true do
           let line = input_line ic in
           (* Print each line to the terminal. *)
           print_endline line;
           (* Also save each line to the log file. *)
           output_string oc (line ^ "\n")
         done
       with End_of_file ->
         (* Close the process cleanly once there is no more output. *)
         ignore (Unix.close_process_in ic)
      );

      (* Close the output file after all lines have been written. *)
      close_out oc;

      (* Rename Fast Downward's default plan file into our own filename. *)
      let default_plan = "sas_plan" in
      let new_plan_name = "solution_plan.txt" in

      if Sys.file_exists default_plan then (
        (* Remove an older result if it already exists. *)
        if Sys.file_exists new_plan_name then Sys.remove new_plan_name;
        (* Move the planner output to the final filename. *)
        Sys.rename default_plan new_plan_name;
        print_endline ("Renamed " ^ default_plan ^ " to " ^ new_plan_name)
      )
      else
        (* Warn when the planner did not create a plan file. *)
        print_endline "Warning: solution_plan was not generated."

let () =
  (* Main: run translator for domain and problem, then run planner.
     This is a simple end-to-end flow used for automated tests. *)
  process_file "./data/domain.pddl";
  process_file "./data/problem_gridindefine.pddl";
  process_file "./data/problem_grid.pddl";

    (* Ensure planner(fast downward) is available *)
  check_planner_installed ();

  (* Run planner automatically *)
  run_planner ()




