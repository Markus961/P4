(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 5
      :columns 5
      :name fileno
      :connections -v -h
      :keys D = diamond T = triangle
      
      :lockedlocations [
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 1 1 1 0]
            [0 0 1 1 1 0]
            [0 0 1 1 0 0]
            ] (shape triangle))

      :keylocations [
            [- - - C - T]
            [- - - - - -]
            [- - - - - -]
            [- - - - - -]
            [- - D S - -]
            [- D - - - T]
            ]
   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)