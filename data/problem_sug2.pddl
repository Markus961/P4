(define (problem strips-grid-y-2)
   (:domain grid)

   ;;Syntactic sugar kunne være :grid 5 5
   (:objects :grid 6 6 
             triangle diamond square circle key0 key1 
             key2 key3 key4 key5 key6 key7 key8 key9)

   ;;initial state
   (:init (arm-empty)

          ;;Places
          (place :grid 6 6)
          
          ;;Shapes
          (shape triangle)
          (shape diamond)
          (shape square)
          (shape circle)

          ;;Connections
          (for-each node in grid
          (for-each neighbor of node
          (if neighbor-adjacent? node neighbor
          (conn node neighbor))))

          ;;Locked nodes
          (locked grid [
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 1 1 1 0]
            [0 0 1 1 1 0]
            [0 0 1 1 0 0]
            ] :shape triangle)

          ;;Open nodes er så de nodes der ikke er locked

          ;;Key location Diamond, Circle, Triangle, Square
          (keylocation grid [
            [- - - C - T]
            [- - - - - -]
            [- - - - - -]
            [- - - - - -]
            [- - D S - -]
            [- D - - - T]
            ])

          (at-robot node5-5)
          
   (:goal (and (at key8 node3-2)
               (at key5 node4-2)
               (at key0 node4-1)))))