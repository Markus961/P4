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

      ;:shapes og :lockednodesarray shape er forskelligt, man kan godt definere square selvom den ikke er i grid :shapes.
      :lockednodesarray fileno ([(3,2) (3,3) (3,4) (4,2) (4,3) (4,4) (5,2) (5,3)] (shape square))
      

   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)