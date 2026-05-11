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

   (:init (arm-empty)
         
        (at-robot fileno1-0))
          
   (:goal (at key1 fileno1-1))
)