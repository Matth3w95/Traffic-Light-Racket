#lang racket

;SOB 10: Contribute to code of end block project
;SOB 3: Enter simple expressions, including nested brackets and symbols bound to values into the interaction window, execute them and explain what is happening.
;SOB 21 : Work effectively as part of a group, demonstrating respect for others and contributing to shared goals
;SOB 103: Work as part of a programming team, engaging in discussion with colleagues in ways that demonstrate an ability to understand other people’s code and offer constructive criticism.
;SOB 144: Work effectively and timely as part of a group to present the results of a project.
;state table conveying sequence of states.
;SOB 217: Write Racket code to implement a given finite state machine.
(define state_table1  '( (("Red" "rdelay") "Red_Orange") (("Red" "reset") "Red")
  (("Red_Orange" "rodelay" ) "Green")
  (("Red_Orange" "reset")  "Red")
  (("Green" "gdelay") "Blinking_Green")
  (("Green" "reset") "Red")
  (("Blinking_Green" "bgdelay") "Orange")
  (("Blinking_Green" "reset") "Red")
  (("Orange" "odelay") "Red")
  (("Orange" "reset") "Red")))


;function defining the newstate and the coniditional arguments that generate the next consecutive state according to the state table.

SOB ;8: Use define, lambda and cond, with other language features as appropriate, to create and use a simple function.
(define next_state0 (λ (s e state_table1)
                (cond
                  
                  ((and (equal? s (caaar state_table1)) (equal? e (cadr(caar state_table)))) (cadr (car state_table1)))
                  (#t (next_state0 s e (rest state_table))))))


;Defining a function with to inputs using lambda.
(define next_state (λ (s e) (next_state0 s e state_table1)))

;Function to run the sequence of states using lambda,cond,recursion.
;SOB 102: Write a simple recursive function to carry out a well-defined task over lists or integers, test the function and explain how it works.
(define run (λ (i s state_table)
              (cond
                ((empty? s) "done")
                (#t (println (next_state0 i (first s) state_table1 ))
                 (sleep 1)
                 (run (next_state0 i (first s) state_table1) (rest s ) state_table1)
                 ))))


(define run-seq (λ (i s) (run i s state_table1)))

;Animation/Visual Representation packages.
(require 2htdp/image)   
(require 2htdp/universe)

;defining constant functions for the background for the images to be used.
(define width 100) 
(define height 100)

;defining a series of traffic lights composed of different coloured outlined circles, solid circles and black rectangles. 

;Image for Red traffic light nested bracket functions to produce an image of a traffic light in a given state.
(define Red (overlay/align "middle" "top" (circle 5 "solid" "red")
                (overlay/align "middle" "middle" (circle 5 "outline" "orange")
                               (overlay/align "middle" "bottom" (circle 5 "outline" "green")
               (rectangle 20 60 "solid" "black")))))

;Image for Blinking Red traffic light
(define Blink-Red(overlay/align "middle" "top" (circle 5 "outline" "red")
               (overlay/align "middle" "middle" (circle 5 "outline" "orange")
                         (overlay/align "middle" "bottom" (circle 5 "outline" "green")
               (rectangle 20 60 "solid" "black")
               (overlay/align "middle" "top" (circle 5 "solid" "red")
                (overlay/align "middle" "middle" (circle 5 "outline" "orange")
                               (overlay/align "middle" "bottom" (circle 5 "outline" "green")
               (rectangle 20 60 "solid" "black"))))))))

;Image for Orange traffic light
(define Orange (overlay/align "middle" "top" (circle 5 "outline" "red")
               (overlay/align "middle" "middle" (circle 5 "solid" "orange")
                   (overlay/align "middle" "bottom" (circle 5 "outline" "green")     
               (rectangle 20 60 "solid" "black")))))

;Image for Blinking Orange traffic light
(define Blink-Orange (overlay/align "middle" "top" (circle 5 "outline" "red")
               (overlay/align "middle" "middle" (circle 5 "outline" "orange")
                         (overlay/align "middle" "bottom" (circle 5 "outline" "green")
               (rectangle 20 60 "solid" "black"))
                         (overlay/align "middle" "top" (circle 5 "outline" "red")
               (overlay/align "middle" "middle" (circle 5 "solid" "orange")
                   (overlay/align "middle" "bottom" (circle 5 "outline" "green")     
               (rectangle 20 60 "solid" "black")))))))

;Image for Green Traffic Light
(define Green(overlay/align "middle" "top" (circle 5 "outline" "red")
               (overlay/align "middle" "middle" (circle 5 "outline" "orange")
                         (overlay/align "middle" "bottom" (circle 5 "solid" "green")
               (rectangle 20 60 "solid" "black")))))

;Image for Blinking Green Traffic Light
(define Blink-Green(overlay/align "middle" "top" (circle 5 "outline" "red")
               (overlay/align "middle" "middle" (circle 5 "outline" "orange")
                         (overlay/align "middle" "bottom" (circle 5 "outline" "green")
               (rectangle 20 60 "solid" "black")
               (overlay/align "middle" "top" (circle 5 "outline" "red")
               (overlay/align "middle" "middle" (circle 5 "outline" "orange")
                         (overlay/align "middle" "bottom" (circle 5 "solid" "green")
               (rectangle 20 60 "solid" "black"))))))))

;Image for Red-Orange Traffic light
(define Red-Orange(overlay/align "middle" "top" (circle 5 "solid" "red")
               (overlay/align "middle" "middle" (circle 5 "solid" "orange")
                         (overlay/align "middle" "bottom" (circle 5 "outline" "green")
               (rectangle 20 60 "solid" "black")))))

;defining a function that for every clock tick places a consecutive imagde of thde traffic light according to state table.
;cond where specific clock checkpoints pertain to a specific image.
;SOB 201: Working individually or in a small group, find at least two aspects of Racket that have not been explicitly taught, research them and demonstrate the ability to use them in sensible ways. Fluency and a deep understanding are not expected, but independent study and a willingness to engage with the literature must be shown.
;With respect to the animate tool, incorporation of multiple packages (2dthp images/universe) use/knowledge of clock ticks and relooping the animation)
;Also use of overlay/overlap tools in conjunctions to creating shapes to make the Traffic lights which were not taught in class.
;SOB 212: Express the type of a higher order function.
(define max-time 90)

(define (time-to-frame time)
  (cond
    [(<= (remainder time max-time) 10) (place-image Red 50 30 (empty-scene 100 60))]
    [(<= (remainder time max-time) 20) (place-image Blink-Red 50 30 (empty-scene 100 60))]
   [ (<= (remainder time max-time) 30) (place-image Red-Orange 50 30 (empty-scene 100 60))]
   [ (<= (remainder time max-time) 40) (place-image Orange 50 30 (empty-scene 100 60))]
    [(<= (remainder time max-time) 50) (place-image Green 50 30 (empty-scene 100 60))]
    [(<= (remainder time max-time) 60) (place-image Blink-Green 50 30 (empty-scene 100 60))]
    [(<= (remainder time max-time) 70) (place-image Red-Orange 50 30 (empty-scene 100 60))]
   [ (<= (remainder time max-time) 80) (place-image Green 50 30 (empty-scene 100 60))]
    [else                            (place-image Red 50 30 (empty-scene 100 60))]))

                        

(animate time-to-frame)

;Animation of above function animate is a higher form function taking the above function as input
(animate time-to-frame)