(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 3
      :columns 2
      :name fileno
      :connections -H
      :keys key0 key1
      :shapes ((St = star))
      
      :lockedlocations fileno ([
            [0]*2
            [0]*2
            [0]*2
            
            ] (shape triangle))

      :keylocations fileno ([
            [St 0]
            [St 0]
            [0 0]
            ])
   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot fileno1-0))
          
   (:goal (at key1 fileno1-1))
)