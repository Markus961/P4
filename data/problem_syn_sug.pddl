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
          (locked nodes [(3,2) (3,3) (3,4) (4,2) (4,3) (4,4) (5,2) (5,3)] :shape triangle)

          ;;Open nodes
          open nodes :rows 0-5 :cols 0-5 :except [(3,2) (3,3) (3,4) (4,2) (4,3) (4,4) (5,2) (5,3)])

          ;;Key location
          (keys
          (key0 diamond node0-0)
          (key1 circle node1-3)
          (key2 square node0-1)
          (key3 triangle node1-5)
          (key4 triangle node5-5)
          (key5 diamond node4-2)
          (key6 square node0-3)
          (key7 diamond node5-1)
          (key8 square node4-3)
          (key9 diamond node5-5)
          )
          (at-robot node5-5)
          
   (:goal (and (at key8 node3-2)
               (at key5 node4-2)
               (at key0 node4-1))))