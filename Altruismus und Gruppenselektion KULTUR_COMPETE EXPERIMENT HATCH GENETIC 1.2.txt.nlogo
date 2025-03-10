;; 1st attempt to include culture: intergenrational transmission of altruism norm
;; 3rd cyan population also has low share of altruists at the beginning. But when older and younger
;; turtles meet, >all< cyan turltles (non-) cooperate as used, but also convert young egoists
;; into young altrist with a given probability
;; next step: spatial concentration of altristst for different groups


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experiments ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. baseline: set conflict and transmission to zero
;; 2. Treatment 1: set only transmission to one
;; 3. Treatment 2: set only conflict to one
;; 4. Treatment 3: "Interaction": set transmission and conflict to one

;; make sliders for experiment: conflict payoff = 0, transmission prob=0
;; loose-conflict: looser-energy variable

;; potential other variations in experiments:
;; age, age difference of cultural transmission: "transmiss-p",
;; probability of cultural transmissionquestion e.g.
;; is transmiss-p of 60% enough? this would be high tolerance for low transmission fidelity
;; age difference: high required difference makes transmission less likely: "age-diff-req", age difference required
;; baseline shares of altruists in red, green, blue group
;; payoff matrix for cooperation and conflict

;; make altruist altruist? variable

breed [persons person]
persons-own [ energy altruist birth-tick age transmiss-p offspring]
globals [share-green share-cyan share-red share-altruists start_n n_pers n_green n_cyan age-diff-req  ]

to setup
  clear-all
  ;set-default-shape persons "person"
  let n 100
  create-persons n [
    set size 1.5
    setxy random-xcor random-ycor
    set color green]
  create-persons n [
    set size 1.5
    setxy random-xcor random-ycor
    set color red]
  create-persons n [
    set size 1.5
    setxy random-xcor random-ycor
    set color cyan]

    ask persons [if color = green [
      ifelse random-float 100 < 60 ;; % Altruists in green group
      [set altruist 1]
      [set altruist 0]
      set transmiss-p 0]
      ]

    ask persons [if color = red [
      ifelse random-float 100 < 40 ;; % Altruists in red group
      [set altruist 1]
      [set altruist 0]
      set transmiss-p 0]
      ]

    ask persons [if color = cyan [
      ifelse random-float 100 < 40 ;; % Altruists in cyan group
      [set altruist 1]
      [set altruist 0]
      set transmiss-p transmiss-global] ;; transmiss-global
      ]

    ask persons [
    set age-diff-req 40
    set energy (10 + random 20)  ; set energy random 10 start with a random amt. of energy
    set birth-tick (random 100)
    set age (0)
    set offspring 0
    ifelse altruist = 1
      [set shape "circle"]
      [set shape "triangle"]
    set start_n n]
    reset-ticks
end

to go
  ;if not any? persons [ stop ] ;mutate;reproduce
  if  count persons <= 5 or ticks >= 1000  [ stop ] ;; or ( n_green / (n_pers + 1) > 0.5 ) or ( n_cyan / (n_pers + 1) > 0.5 )
  ask persons
  [ move
    epidemic
      ]
  update-globals
  tick
end

to move  ;; person procedure
  rt random 50
  lt random 50
  fd 1
  ;set energy energy - 0.5
  let meet persons with [any? other persons-here] ;; meet is a local variable  [any? neighbors4]
  if any? meet [interact]
  let energ floor(energy)
  set label energ; age energ altruist birth-tick transmiss-p offspring
  set age ((ticks + 100) - birth-tick)
  if energy > (60 + random 10) and age > (20 + random 10) [reproduce]
  ifelse ((age > (1000 + random 200) or (energy < -50)) and random 100 < 50)  [die] [move-to-empty]
  if count persons > (start_n * 2) [epidemic]
end

;; interaction is programmed from the alteri perspective
;; persons-on neighbors4 turtles-on patch-ahead 1 other turtles-here

to interact
  ask other persons-here  [
     ifelse color = [color] of myself
      [cooperate]
      [conflict]
  ]
end

;; cooperation is programmed from the alteri perspective
;; carefully check payoff matrix and view effects in world
;; not a real PD since loss due to defector is not suckers-payoff.
;; Rather "by-product mutualism" or "sefish teamwork" (Dugatkin 1999: 21, 108 )

to cooperate  ;other turtles-here ask other persons-here [set energy energy + 3]
  ask other persons-here [ ;; persons-on neighbors4 persons-here
    print "cooperate"
    if altruist = 1 and color = [color] of myself
      [ifelse [altruist] of myself = 1
        [ ask myself [ set energy energy + 3 ] set energy energy + 3] ;; Ego & Alter
        [ ask myself [ set energy energy + 5 ] set energy energy - 1] ];; Alter (who is Ego in this procedure!)
    if  altruist = 0 and color = [color] of myself
      [ifelse [altruist] of myself = 1
        [ ask myself [ set energy energy - 1 ] set energy energy + 5] ;; Ego Ego & Alter
        [ ask myself [ set energy energy - 1 ] set energy energy - 1] ]
    ]
  ; culture here: elder  socializes younger into altruist with [transmiss-p] probability, only altruists spread the meme, age difference plays a role
  ask other persons-here [
    if [color] of myself = color and (age - age-diff-req) > [age] of myself and altruist = 1 and random 100 < transmiss-p ;;and other persons-here != nobody 27
    [ask myself [ set altruist 1 set shape "person"] ]
    if [color] of myself = color and (age + age-diff-req) < [age] of myself and [altruist] of myself = 1 and random 100 < transmiss-p ;;and other persons-here != nobody
    [ set altruist 1 set shape "person"]
  ]
  if  other persons-here = nobody [move]
  ask persons [move-to-empty]
end
;; but carefully check inclusion/exclusion of statements in/out of brackets!

;; conflict is programmed from the alteri perspective
;; add probability of winning/losing in future versions
to conflict
  print "conflict"
  ask other persons-here [ ;;persons-on  neighbors4 persons-here turtles-on patch-ahead 1
    if energy >= [energy] of myself and color != [color] of myself [move-to-empty set energy energy - 1]
    if energy < [energy] of myself and color != [color] of myself [loose-conflict]
    move-to-empty]
    if other persons-here = nobody [move]
    ;; conflict: the more powerful sucks energy from less powerful
end

to loose-conflict
 set energy energy - looser-energy ;;;  persons [die] looser-energy
  ask persons [move-to-empty]
end

to move-to-empty
    let meet false
    if any? other persons-here  ;; other turtles-hereother persons-on neighbors4
    [ move-to max-one-of (patches with [not any? persons-here]) [distance myself]   ] ;min-one-of max-one-of => also condition of interest
end ;min-one-of to check world, but should be max-one-of

;; should be done within each person-step? ;; give birth to a new person, but it takes some of energy
to reproduce     ;; person
  ask persons [
    ifelse energy > (60 + random 10) and age > (20 + random 10);set color yellow
      [set energy 10 hatch 1 [
                              move-to one-of patches with [not any? persons-here]
                              set energy (10 + random 20)
                              set birth-tick ticks
                              set age (ticks + 10 - birth-tick)
                              set offspring 1
                              print "reproduce"
        ;; transmission bias here: in cyan group transmission to offspring is genetic first => 40% altruists again
                              ifelse ( offspring = 1 and color = cyan and random 100 < 40 ) [set altruist 1] [set altruist 0]
        ;; transmission bias of 20% error here:
        ;; ifelse [altruist] of myself = 1 and random 100 > 20 [set altruist = 1] [set altruist = 0]
        ;; ifelse [altruist] of myself = 0 and random 100 > 20 [set altruist = 0] [set altruist = 1]
                              ]]
                              [move-to-empty]
  ]
end

;; 25% die randomly if population is larger than factor x
to epidemic
  let n-epi random 100
  if count persons > (start_n * 4) [
    ifelse n-epi < 25 [die ] [move]  ]
end

to update-globals
  set n_pers count persons
  set n_green count persons with [color =[green]]
  set n_cyan count persons with [color = [cyan]]

  let n-red count persons with [color = red]
  let n-green count persons with [color = green]
  let n-cyan count persons with [color = cyan]
  let n-altruists count persons with [altruist = 1]
  let n-green-altruists count persons with [color = green and altruist = 1]
  let n-green-egoist count persons with [color = green and altruist = 0]
  let n-red-altruists count persons with [color = red and altruist = 1]
  let n-red-egoist count persons with [color = red and altruist = 0]
  let n-cyan-altruists count persons with [color = cyan and altruist = 1]
  let n-all count persons
  set share-green (n-green / (n-all + 0.1)) * 100
  set share-cyan (n-cyan / (n-all + 0.1)) * 100
  set share-red (n-red / (n-all + 0.1)) * 100
  set share-altruists (n-altruists / (n-all + 0.1)) * 100
end

;; go ahead with meaningful experients, e.g. set conflict effect to zero
@#$#@#$#@
GRAPHICS-WINDOW
1054
34
1846
827
-1
-1
19.122
1
10
1
1
1
0
1
1
1
-20
20
-20
20
0
0
1
ticks
30.0

BUTTON
72
666
127
699
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
135
667
190
700
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
653
846
947
891
NIL
count persons with [color = red and altruist = 1]
17
1
11

MONITOR
651
789
945
834
NIL
count persons with [color = green and altruist = 1]
17
1
11

PLOT
70
24
941
619
plot 1
time
percent
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"%-green" 1.0 0 -13840069 true "plot share-green" "if ticks > 50 [plot share-green]"
"%-altruists" 1.0 0 -1184463 true "" "if ticks > 50 [plot share-altruists]"
"%-cyan" 1.0 0 -6759204 true "" "if ticks > 50 [plot share-cyan]"
"%-red" 1.0 0 -2674135 true "" "if ticks > 50 [plot share-red]"

MONITOR
650
732
944
777
NIL
count persons with [color = cyan and altruist = 1]
17
1
11

MONITOR
742
680
942
725
NIL
count persons with [color = cyan]
17
1
11

MONITOR
850
627
942
672
NIL
count persons
17
1
11

MONITOR
635
626
838
671
NIL
count persons with [offspring = 1]
17
1
11

SLIDER
80
773
252
806
transmiss-global
transmiss-global
0
100
80.0
10
1
NIL
HORIZONTAL

SLIDER
79
832
251
865
looser-energy
looser-energy
0
5
5.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This project explores a simple ecosystem made up of rabbits, grass, and weeds. The rabbits wander around randomly, and the grass and weeds grow randomly.   When a rabbit bumps into some grass or weeds, it eats the grass and gains energy. If the rabbit gains enough energy, it reproduces. If it doesn't gain enough energy, it dies.

The grass and weeds can be adjusted to grow at different rates and give the rabbits differing amounts of energy.  The model can be used to explore the competitive advantages of these variables.

## HOW TO USE IT

Click the SETUP button to setup the rabbits (red), grass (green), and weeds (violet). Click the GO button to start the simulation.

The NUMBER slider controls the initial number of rabbits. The BIRTH-THRESHOLD slider sets the energy level at which the rabbits reproduce.  The GRASS-GROWTH-RATE slider controls the rate at which the grass grows.  The WEEDS-GROWTH-RATE slider controls the rate at which the weeds grow.

The model's default settings are such that at first the weeds are not present (weeds-grow-rate = 0, weeds-energy = 0).  This is so that you can look at the interaction of just rabbits and grass.  Once you have done this, you can start to add in the effect of weeds.

## THINGS TO NOTICE

Watch the COUNT RABBITS monitor and the POPULATIONS plot to see how the rabbit population changes over time. At first, there is not enough grass for the rabbits, and many rabbits die. But that allows the grass to grow more freely, providing an abundance of food for the remaining rabbits. The rabbits gain energy and reproduce. The abundance of rabbits leads to a shortage of grass, and the cycle begins again.

The rabbit population goes through a damped oscillation, eventually stabilizing in a narrow range. The total amount of grass also oscillates, out of phase with the rabbit population.

These dual oscillations are characteristic of predator-prey systems. Such systems are usually described by a set of differential equations known as the Lotka-Volterra equations. NetLogo provides a new way of studying predatory-prey systems and other ecosystems.

## THINGS TO TRY

Leaving other parameters alone, change the grass-grow-rate and let the system stabilize again.  Would you expect that there would now be more grass?  More rabbits?

Change only the birth-threshold of the rabbits.  How does this affect the steady-state levels of rabbits and grass?

With the current settings, the rabbit population goes through a damped oscillation. By changing the parameters, can you create an undamped oscillation? Or an unstable oscillation?

In the current version, each rabbit has the same birth-threshold. What would happen if each rabbit had a different birth-threshold? What if the birth-threshold of each new rabbit was slightly different from the birth-threshold of its parent? How would the values for birth-threshold evolve over time?

Now add weeds by making the sliders WEEDS-GROW-RATE the same as GRASS-GROW-RATE and WEEDS-ENERGY the same as GRASS-ENERGY.  Notice that the amount of grass and weeds is about the same.

Now make grass and weeds grow at different rates.  What happens?

What if the weeds grow at the same rate as grass, but they give less energy to the rabbits when eaten (WEEDS-ENERGY is less than GRASS-ENERGY)?

Think of other ways that two plant species might differ and try them out to see what happens to their relative populations.  For example, what if a weed could grow where there was already grass, but grass couldn't grow where there was a weed?  What if the rabbits preferred the plant that gave them the most energy?

Run the model for a bit, then suddenly change the birth threshold to zero.  What happens?

## NETLOGO FEATURES

Notice that every black patch has a random chance of growing grass or
weeds each turn, using the rule:

    if random-float 1000 < weeds-grow-rate
      [ set pcolor violet ]
    if random-float 1000 < grass-grow-rate
      [ set pcolor green ]

## RELATED MODELS

Wolf Sheep Predation is another interacting ecosystem with different rules.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (2001).  NetLogo Rabbits Grass Weeds model.  http://ccl.northwestern.edu/netlogo/models/RabbitsGrassWeeds.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2001 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227.

<!-- 2001 -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

rabbit
false
0
Circle -7500403 true true 76 150 148
Polygon -7500403 true true 176 164 222 113 238 56 230 0 193 38 176 91
Polygon -7500403 true true 124 164 78 113 62 56 70 0 107 38 124 91

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="conflict_culture" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>share-green</metric>
    <metric>share-cyan</metric>
    <metric>share-red</metric>
    <enumeratedValueSet variable="looser-energy">
      <value value="1"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transmiss-global">
      <value value="0"/>
      <value value="40"/>
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
