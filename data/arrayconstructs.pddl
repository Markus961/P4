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

      ;Nu fungere det sådan at det man definere i grid og i :shapes er det man kan bruge, så hvis man skriver shape square i
      ;lockednodesarray så får man en parser fejl, fordi :shapes i grid er vores verden, så det er kun dem man kan bruge i sit grid.
      ;Mangler open nodes bliver parset automatisk, når man definere lockednodesarray.

      :lockednodesarray fileno ([(3,2) (3,3) (3,4) (4,2) (4,3) (4,4) (5,2) (5,3)] (shape diamond))
      

   )

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)