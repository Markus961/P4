(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 6
      :columns 6
      :name fileno
      :connections -H -V
      :keys key0 key1 key2 key3 key4 key5 key6 key7 
      :shapes ((D = diamond) (T = triangle) (St = star) (S = square))
      
      :lockedlocations fileno ([
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 1 1 1 0]
            [0 0 1 1 1 0]
            [0 0 1 1 0 0]
            ] (shape triangle))

      :keylocations fileno ([
            [St 0 D 0 0 T]
            [S 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 D T 0 0]
            [0 D 0 0 0 T]
            ])
   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot fileno5-5))
          
   (:goal (at key7 fileno3-2))
)