(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects test_object)

   (:grid 
      :rows 5
      :columns 5
      :name fileno
      :connections -v -h
      :keys ((D = diamond) (T = triangle))
      
      :lockedlocations ([
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 1 1 1 0]
            [0 0 1 1 1 0]
            [0 0 1 1 0 0]
            ] (shape triangle))

      :keylocations ([
            [0 0 0 C 0 T]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]
            [0 0 D S 0 0]
            [0 D 0 0 0 T]
            ])
   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)