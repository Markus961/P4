(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 6
      :columns 6
      :name fileno
      :connections -H -V
      :keys key0 key1 key2 
      :shapes ((T = triangle) (D = diamond))

      ; The :shapes in the grid section is what you can use in the lockednodesarray
      ; This array structure parses locked and open nodes

      :lockednodesarray fileno ([(3,2) (3,3) (3,4) (4,2) (4,3) (4,4) (5,2) (5,3)] (shape diamond))

      :keylocations fileno ([
            [T 0 0 0 0 0]
            [D 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 T]
            ])
      

   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot fileno5-5))
          
   (:goal (at key2 fileno3-2))
)