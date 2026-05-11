(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 2
      :columns 6
      :name test
      :connections 
      :keys key0 
      :shapes ()
      
      :lockedlocations test ([
            [0]*3 + [1]*3
            [0]*3 + [1]*3
            ] (shape triangle))

   )

   (:init (arm-empty)
         
        (at-robot test0-1))
          
   (:goal (at key0 test0-0))
)