(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 5
      :columns 5
      :name fileno
      :connections -v -h
      :keys key0 key1 key2 key3 key4 key5 key6 key7
      :Shapes ((D = diamond) (T = triangle) (St = star) (S = square))
      
      :lockedlocations fileno ([
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 1 1 1 0]
            [0 0 1 1 1 0]
            [0 0 1 1 0 0]
            ] (shape triangle))

      :keylocations fileno ([
            [St 0 0 D 0 T]
            [S 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 D T 0 0]
            [0 D 0 0 0 T]
            ])
   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)