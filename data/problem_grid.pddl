(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 2
      :columns 6
      :name fileno
      :connections -H -V
      :keys key0 key1
      :shapes ((St = star))
      
      :lockedlocations fileno ([
            [0]*3 + [1]*3
            [0]*3 + [1]*3
            
            ] (shape triangle))

      :keylocations fileno ([
            [St 0 0 0 0 0]
            [St 0 0 0 0 0]
            ])
   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)