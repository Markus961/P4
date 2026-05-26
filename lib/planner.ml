let planner_path = ref None

let find_home_dir () =
  (* For Mac *)
  match Sys.getenv_opt "HOME" with
  | Some home -> Some home
  (* For Windows *)
  | None -> Sys.getenv_opt "USERPROFILE"

let check_planner_installed () =
  (* Allow the planner path to be set explicitly on any system. *)
  match Sys.getenv_opt "FAST_DOWNWARD_PATH" with
  | Some custom_path when Sys.file_exists custom_path ->
      (* Store the custom path so run_planner can use it later. *)
      planner_path := Some custom_path
  | Some custom_path ->
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
            planner_path := Some path
          else (
            print_endline "\nERROR: Fast Downward not found.";
            print_endline ("Expected at: " ^ path);
            print_endline "Set FAST_DOWNWARD_PATH or see PLANNER_SETUP.md.";
            exit 1
          ))


let run_planner () =
  match !planner_path with
  | None ->
      print_endline "Planner path not set.";
      exit 1
  | Some path ->
      print_endline "\nRunning Fast Downward...";

      (* File where we store the full planner log. *)
      let output_filename = "detailed_solution_plan.txt" in

      (* Build the command that runs Fast Downward on the translated PDDL files. *)
      let planner_exec =
        (* Windows needs an explicit python call for the .py script. *)
        if Sys.os_type = "Win32" then
          "python \"" ^ path ^ "\""
        (* mac can run the script directly. *)
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
           print_endline line;
           output_string oc (line ^ "\n")
         done
       with End_of_file ->
         ignore (Unix.close_process_in ic)
      );

      close_out oc;

      (* Rename Fast Downward's default plan file into our own filename. *)
      let default_plan = "sas_plan" in
      let new_plan_name = "solution_plan.txt" in

      if Sys.file_exists default_plan then (
        if Sys.file_exists new_plan_name then Sys.remove new_plan_name;
        Sys.rename default_plan new_plan_name;
        print_endline ("Renamed " ^ default_plan ^ " to " ^ new_plan_name)
      )
      else
        print_endline "Warning: solution_plan was not generated."