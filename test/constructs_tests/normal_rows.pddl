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
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)