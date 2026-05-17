(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 3
      :columns 2
      :name fileno
      :connections -H
      :keys key1
      :shapes ((D = diamond))
      
   )

   (:grid 
      :rows 1
      :columns 6
      :name grid2
      :connections -H -V
      :keys key0
      :shapes ((St = star))
      
      :lockedlocations grid2 ([
            [0]*3 + [1]*3 ; problem at + tolkes som ny row tjek parser
            
            ] (shape star))

      :keylocations grid2 ([
            [St 0 0 0 0 0]
            ])
   )

   (:init (arm-empty)
         
        (at-robot grid20-1))
          
   (:goal (at key0 grid20-5))
)