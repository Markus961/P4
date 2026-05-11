(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 1
      :columns 6
      :name test
      :connections 
      :keys key0 
      :shapes ()
      
      :lockedlocations test ([
            [0 0 0 1 1 1]
            ] (shape triangle))

   )

   (:init (arm-empty)
         
        (at-robot test0-1))
          
   (:goal (at key0 test0-0))
)