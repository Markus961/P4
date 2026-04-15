(define (problem strips-grid-y-2)
   (:domain grid)

   (:objects (:grid 10 10 bazooka)
             triangle diamond square circle key0 key1 key2
             key3 key4 key5 key6 key7 key8 key9)

   (:init (arm-empty)
         
        ;;virker ikke uden denne og goal
        (at-robot node5-5))
          
   (:goal (at key8 node3-2))
)