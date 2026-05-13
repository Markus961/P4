# P4
Software P4 Project
OCaml Compiler, adding extra functionality to PDDL with :grid syntax to make PDDL with a lot of places less time-intensive and repetetive.
WE DID NOT MAKE PDDL, RATHER THE :grid SYNTAX EXTENSION.

**UNFINISHED README**

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
The Rows parameter can be *any non-negative number*

### :columns
The Columns parameter can be *any non-negative number*

### :name
The Name parameter can be *any name, including numbers, except with a number as the first character*

### :connections
The Connections parameter has "flags" which generate certain connections upon compiling

-V adds vertical lines (node0-0 to node0-1 and node0-1 to node0-2)
-H adds horizontal lines (node0-0 to node1-0 and node1-0 to node2-0)
-D adds diagonal lines (node0-0 to node1-1 and node1-1 to node2-2)
-W makes all lines wrap around, so from the last node to the first. If combined with -H and a 2x2 grid (node2-0 to node0-0)

### :keys
**Not Done**

### :shapes
**Not Done**

### :lockedlocations
**Not Done**

### :keylocations
**Not Done**

## Grid Syntax Example
Example Grid Definitions could Look as Follows:

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