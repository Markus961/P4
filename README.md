# P4
Software P4 Project
OCaml Compiler, adding extra functionality to PDDL with :grid syntax to make PDDL with a lot of places less time-intensive and repetitive.
WE DID NOT MAKE PDDL, RATHER THE :grid SYNTAX EXTENSION.

**UNFINISHED README**

# Running the Compiler
To run the compiler, place yourself in the "P4" folder (The outermost folder) and execute **"dune build"** followed by **"dune exec grid_extension"**. The output files are called **"transformed_domain.pddl"** and **"transformed_problem.pddl"**
To run tests type **"dune runtest"**

# Running the PDDL Planner with Files Generated from Compiler
See: PLANNER_SETUP.md

# Syntax and Related Semantics
This part will not explain PDDL syntax and semantics, only the :grid syntax extension, therefore you might require some PDDL syntax knowledge.

## :grid
The :grid keyword is to be placed in between the :objects and :init sections

Note: There can be zero or more grids

(:objects \*Objects\*)

*(:grid \*Grid Syntax\*)*

(:init \*Initial State\*)

## Grid Parameters

A grid is placed and used with the parameters below:

:rows *(Required)*
(The Number of Rows in the Grid)

:columns *(Required)*
(The Number of Columns in the Grid)

:name *(Required)*
(The Name of the Grid)

:connections
(Flags for How Nodes in the Grid are Connected)

:keys
(The Key Objects Later Attributed to Shapes in :shapes and :keylocations)

:shapes
(The Different Key Shapes or Types a Key can have)

:lockedlocations
(A Matrix Detailing which Locations are Locked by which Shape)

:keylocations
(A Matrix Detaling where Each Key is)


### :rows
The Rows parameter can be *any positive number*

### :columns
The Columns parameter can be *any positive number*

### :name
The Name parameter can be *any name, including numbers, except with a number as the first character*

### :connections
The Connections parameter has "flags" which generate certain connections upon compiling

-V adds vertical lines (node0-0 to node0-1 and node0-1 to node0-2)
-H adds horizontal lines (node0-0 to node1-0 and node1-0 to node2-0)

### :keys

### :shapes

### :lockedlocations

### :keylocations

## Grid Syntax Example
Example Grid Definitions could Look as Follows:

```
(:grid 
      :rows 1
      :columns 6
      :name gridexample
      :connections -H
      :keys key0
      :shapes ((St = star))
      
      :lockedlocations gridexample ([
            [0 0 0 1 1 1]
            
            ] (shape star))

      :keylocations gridexample ([
            [St 0 0 0 0 0]
            ])
)
```

Another example with slightly different but correct syntax:

```
(:grid 
      :rows 6
      :columns 6
      :name gridexample2
      :connections -H -V
      :keys key0 key1 key2 key3 key4 key5
      :shapes ((T = triangle))
      
      :lockedlocations gridexample2 ([
            [0]*3 + [1]*3
            [0]*6
            [0]*6
            [0]*3 + [1] + [0]*2
            [0]*6
            [0]*6
            
            ] (shape triangle))

      :keylocations gridexample2 ([
            [T T T 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 T 0]
            ])
)
```

# Injected / Inferred Predicates and Actions

The compiler automatically adds some predicates and actions that are usable without the user manually typing them. The predicates are added to make sure the compiler runs correctly with the correctly defined names and number of arguments. The actions are added for ease of use, and to also give a set of default instructions for the AI planner to find a valid solution.

The intjected / inferred predicates are:
```
(conn ?x ?y)
(key-shape ?k ?s)
(lock-shape ?x ?s)
(at ?r ?x)
(at-robot ?x)
(place ?p)
(key ?k)
(shape ?s)
(locked ?x)
(holding ?k)
(open ?x)
(arm-empty )
(reachable ?x)
```

The intjected / inferred actions are:
```
(:action move
      :parameters (?from ?to)
      :precondition (and
            (place ?from)
            (place ?to)
            (at-robot ?from)
            (conn ?from ?to)
      )
      :effect (and
            (not (at-robot ?from))
            (at-robot ?to)
      )
)

(:action unlock
      :parameters (?curpos ?lockpos ?key ?shape)
      :precondition (and
            (place ?curpos)
            (place ?lockpos)
            (key ?key)
            (shape ?shape)
            (conn ?curpos ?lockpos)
            (key-shape ?key ?shape)
            (lock-shape ?lockpos ?shape)
            (at-robot ?curpos)
            (locked ?lockpos)
            (holding ?key)
      )
      :effect (and
            (open ?lockpos)
            (not (locked ?lockpos))
      )
)

(:action pickup
      :parameters (?curpos ?key)
      :precondition (and
            (place ?curpos)
            (key ?key)
            (at-robot ?curpos)
            (at ?key ?curpos)
            (arm-empty)
      )
      :effect (and
            (holding ?key)
            (not (at ?key ?curpos))
            (not (arm-empty))
      )
)

(:action pickup-and-loose
      :parameters (?curpos ?newkey ?oldkey)
      :precondition (and
            (place ?curpos)
            (key ?newkey)
            (key ?oldkey)
            (at-robot ?curpos)
            (holding ?oldkey)
            (at ?newkey ?curpos)
      )
      :effect (and
            (holding ?newkey)
            (at ?oldkey ?curpos)
            (not (holding ?oldkey))
            (not (at ?newkey ?curpos))
      )
)

(:action putdown
      :parameters (?curpos ?key)
      :precondition (and
            (place ?curpos)
            (key ?key)
            (at-robot ?curpos)
            (holding ?key)
      )
      :effect (and
            (arm-empty)
            (at ?key ?curpos)
            (not (holding ?key))
      )
)

```