Setup Guide for External AI Planner (Fast Downward)

This guide explains how to:

Install required external tools
(Optional) Validate transformed PDDL files
Install the external AI planner (Fast Downward)
Run planning and obtain a solution
Project Pipeline Overview

Our system follows this pipeline:

PDDL Extension
→ OCaml Compiler
→ Standard PDDL
→ External AI Planner
→ Plan

The planner is not part of our project.
It must be installed separately.

Step 1 – Install Required Tools

Our project depends on external tools:

Fast Downward (required)
cpddl (optional, for validation only)
1.1 Install CMake (Required for Fast Downward)

Check if CMake is installed:

cmake --version

If not installed:

brew install cmake

If Homebrew is not installed:

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
Step 2 – Install Fast Downward (Required)

Fast Downward is the actual AI planner.

⚠ It must NOT be placed inside the P4 repository.

Download and Build

Go to your home directory:

cd ~
mkdir -p planners
cd planners

Clone Fast Downward:

git clone https://github.com/aibasel/downward.git
cd downward

Build it:

./build.py

This compiles:

Search engines
Heuristics
Translator

The build takes 2–5 minutes.

After this, the planner is ready.

Step 3 – (Optional) Install cpddl for Validation

cpddl is only used to:

Check syntax
Ground the task
Verify reachability

It is NOT required for planning.

If you want to use cpddl, install it separately following its repository instructions.

Then you can validate:

/Users/<YOUR_NAME>/cpddl/bin/pddl transformed_domain.pddl transformed_problem.pddl

If it prints:

Goal is unreachable: 0

That means the goal is reachable.

Step 4 – Run Planning

From your P4 project directory:

~/planners/downward/fast-downward.py \
transformed_domain.pddl \
transformed_problem.pddl \
--search "astar(lmcut())"
What This Does
astar → optimal search
lmcut → admissible heuristic
Guarantees minimal-cost plan
Expected Output

You should see something like this in the terminal: 

pickup fileno5-5 key7
move fileno5-5 fileno4-5
...
Plan length: 7
Plan cost: 7
Solution found.

 as well as 2 files in the P4 repo called "detailed_solution_plan.txt" and "solution_plan.txt" :
 
This confirms:

✔ Transformation works
✔ Planner works
✔ Goal is reachable

Common Errors
cmake not found

Install CMake via brew.

Permission denied in /planners

Use:

~/planners

NOT:

/planners
fast-downward.py not found

Ensure it exists in:

~/planners/downward/
Important

Fast Downward only needs to be installed once per machine.

After installation, you can simply run:

dune exec grid_extension

If correctly configured, the planner will run automatically.