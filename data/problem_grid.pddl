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
      
   )

   (:grid 
      :rows 1
      :columns 6
      :name grid2
      :connections -H -V
      :keys key0
      :shapes ((St = star))
      
      :lockedlocations fileno ([
            [0]*3 + [1]*3 ;; problem at + tolkes som ny row tjek parser
            [0]*3 + [1]*3
            
            ] (shape triangle))

      :keylocations fileno ([
            [St 0 0 0 0 0]
            ])
   )

   (:init (arm-empty)
         
        (at-robot fileno1-0))
          
   (:goal (at key1 fileno1-1))
)