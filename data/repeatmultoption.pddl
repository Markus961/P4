(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)


   (:grid 
      :rows 2
      :columns 6
      :name game
      :connections -H -V
      :keys key0 key1
      :shapes ((St = star) (D = diamond))
      
      :lockedlocations game ([
            [0]*3 + [1]*3
            [0]*6
            ] (shape star))

      :keylocations game ([
            [St]*1 + [0]*5
            [0]*5 + [D]*1
            ])
   )

   (:init (arm-empty)
         
        (at-robot game0-1))
          
   (:goal (at key0 game1-5))
)