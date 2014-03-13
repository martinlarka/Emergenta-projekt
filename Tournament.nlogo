turtles-own [own_history opponent_history points num_wins]

to setup
  ;; Create turtles and give them strategies
  clear-all
  crt num_strategies
    [ set size 0.1  ;; easier to see
      setxy random-xcor random-ycor 
      set own_history []
      repeat num_strategies [set own_history lput [] own_history]
      set opponent_history []
      repeat num_strategies [set opponent_history lput [] opponent_history]]
  reset-ticks
end

to go
  ask turtles [foreach (n-values num_strategies [?]) [challenge who ?]]
  tick
end

to challenge [turtle_x turtle_y]
  ;; get timestep from turtleX
  let x_move calc-move turtle_x (item turtle_y own_history) (item turtle_y opponent_history) 
  
  let y_move calc-move turtle_y (item turtle_y opponent_history) (item turtle_y own_history)
  
  ;; get timestep from turtleY
  ;;let y_move -1
  ;;ask turtle turtle_y [set y_move calc-move turtle_x]
  
  ;; Calculate winner
  ifelse x_move = 10 and  y_move = 10
  ;; Draw
  [;; If x-move and y-move == 10
    set points points + 5
    set num_wins num_wins + 1
  ]
  ;; There is a winner
  [if x_move < y_move 
  ;; Turtle_x wins
    [
     ;; Increase points
     set points (points + x_move)
     ;; Uppdate num-wins
     set num_wins (num_wins + 1)
    ]
  ;; Turtle_y wins do nothing
  
   ]
  ;; Uppdate history on turtles
  set own_history replace-item turtle_y own_history (fput x_move item turtle_y own_history)
  set opponent_history replace-item turtle_y opponent_history (fput y_move item turtle_y opponent_history)
  
end

to-report calc-move [strategy own_hist op_hist]
  if strategy = 0 [report tits-for-twat own_hist op_hist]
  if strategy = 1 [report tits-for-twat2 own_hist op_hist]
  if strategy = 2 [report random-dude own_hist op_hist]
  if strategy = 3 [report its-something-guy own_hist op_hist]
  if strategy = 4 [report scumbag-steve own_hist op_hist]
  if strategy = 5 [report scumbag-stacy own_hist op_hist]
  if strategy = 6 [report good-guy-greg own_hist op_hist]
  if strategy = 7 [report neil-degrasse-tyson own_hist op_hist]
  if strategy = 8 [report robocop own_hist op_hist]
  if strategy = 9 [report close-enought-guy own_hist op_hist]
  if strategy = 10 [report even-numbers-guy own_hist op_hist]
  if strategy = 11 [report loler-guy own_hist op_hist]
  if strategy = 12 [report median-guy own_hist op_hist]
  if strategy = 13 [report grudger own_hist op_hist]
  if strategy = 14 [report adjust-guy own_hist op_hist]
end

to-report get-opponent-history [opponent]
   report item opponent opponent_history
end

to-report get-own-history [opponent]
  report item opponent own_history
end

to-report result-list [own_hist op_hist len]
  
  let result_list []
  let own_hist_sub sublist own_hist 0 len
  let op_hist_sub sublist op_hist 0 len
 
  foreach (n-values length own_hist_sub [?])[
    if (item ? own_hist_sub) > (item ? op_hist_sub) [
      ;;seger
      set result_list lput -1 result_list
    ]
    if item ? own_hist_sub < item ? op_hist_sub [
      ;;förlust
      set result_list lput 1 result_list
    ]
    if item ? own_hist_sub = item ? op_hist_sub [
      ;;lika
      set result_list lput 0 result_list
    ]
  ]
  report result_list
end


;;; Strategies - reports move from 1-10

;; Tits for twat!     (Håll hela första, sedan släppa steget innan den andra släppte)
to-report tits-for-twat [own_hist op_hist]

  ;; Kolla om listan är tom
  ifelse empty? op_hist[
    report 10
  ][
    ;; Hämta resultatlista
    let results result-list own_hist op_hist length own_hist
    ;; Hämta motståndarens senaste drag
    let pos position -1 results
    if pos = false[
      report 10
    ]
    if item pos op_hist = 10[
      report 10
    ]
    if item pos op_hist <= 2[
      report 1
    ]
    report item pos op_hist - 1
  ]
  
end

;; Tits for twat II!   (Håll hela första, sedan släppa två steg innan den andra släppte)
to-report tits-for-twat2 [own_hist op_hist]
  
  ;; Kolla om listan är tom
  ifelse empty? op_hist[
    report 10
  ][
    ;; Hämta resultatlista
    let results result-list own_hist op_hist length own_hist
    ;; Hämta motståndarens senaste drag
    let pos position -1 results
    if pos = false[
      report 10
    ]
    if item pos op_hist = 10[
      report 10
    ]
    if item pos op_hist <= 2[
      report 1
    ]
    report item pos op_hist - 2
  ]
end

;; Random dude!    (rnd(1,10))
to-report random-dude [own_hist op_hist]
  report (random 9) + 1
end

;; It’s something guy   (1-1-1-...-1)
to-report its-something-guy [own_hist op_hist]
  report 1
end

;; Scumbag Steve   (5-4-3-2-1-5-4-3-2-1...)
to-report scumbag-steve [own_hist op_hist]
  
  ;; Kolla om listan är tom
  ifelse empty? own_hist[
    report 5
  ][
    ;; Hämta sin egen senaste move
    ifelse first own_hist = 1[
      report 5
    ][
      report (first own_hist) - 1
    ]
  ]
end

;; Scumbag Stacy  (5-3-1-5-3-1-5-3-1...)
to-report scumbag-stacy [own_hist op_hist]
  
  ;; Kolla om listan är tom
  ifelse empty? own_hist[
    report 5
  ][
    ;; Hämta sin egen senaste move
    ifelse first own_hist = 1[
      report 5
    ][
      report (first own_hist) - 2
    ]
  ]
end

;; Good guy Greg   (10-10-10-...-10)
to-report good-guy-greg [own_hist op_hist]
  report 10
end

;; Neil Degrasse Tyson   (börjar på mitten, mean(opponent-plays))
to-report neil-degrasse-tyson [own_hist op_hist]
  
  ;; Kolla om listan är tom
  ifelse empty? op_hist[
    report 5
  ][
    ;; Hämta sin egen senaste move
    let results result-list own_hist op_hist length own_hist
    let opponent-total 0
    foreach (n-values length results [?])[
      if item ? results = -1[
        set opponent-total opponent-total + item ? op_hist
      ]
    ]
    if length filter [? = -1 ] results != 0[
      report round opponent-total / length filter [? = -1 ] results
    ]
    report 5
  ]
end

;; Robocop  (5-5-5-...-5)
to-report robocop [own_hist op_hist]
  report 5
end

;; Close enough guy  (kör mitten de 3 första omgångarna, Medelvärdet av motståndarens 3 senaste actions i släpphistorik)
to-report close-enought-guy [own_hist op_hist]
  ;; Kolla om listan är tom
  ifelse empty? op_hist[
    report 5
  ][
    ;; Hämta motståndarens tre senaste moves
    let results result-list own_hist op_hist length own_hist
    show "resultat-lista"
    show results
    let three-results []
    foreach (n-values length results [?])[
      if item ? results = -1[
        set three-results lput item ? op_hist three-results
        if length three-results = 3[
          show "egna resultat"
          show own_hist
          show "motståndarens resultat"
          show op_hist
          show "tre senaste"
          show three-results
          show "raporterar medlevärdet av 3 senaste"
          show round mean three-results
          show "end round"
          report round mean three-results
        ]
      ]
    ]
    show "5"
    show "end round"
    report 5
  ]
end

;; Even numbers guy  (rnd(1,5)*2)  //Fungerar (Emil)
to-report even-numbers-guy [own_hist op_hist]
  report ((random 4) + 1) * 2
end

;; Loler-guy    (kör på random tills motståndaren vunnit en gång, tar sedan värdet under motståndarens typvärde)
to-report loler-guy [own_hist op_hist]
  ;; Kolla om listan är tom
  ifelse empty? op_hist[
    
    ;; ENDAST FÖR TEST 
    let result (random 9) + 1
    show "lolerboy returns: "
    show result
    report result
    ;; //ENDAST FÖR TEST 
    ;;report (random 9) + 1
  ][
    ;; Hämta sin egen senaste move
    let results result-list own_hist op_hist length own_hist
    let opp-results []
    foreach (n-values length results [?])[
      if item ? results = -1[
        set opp-results lput item ? op_hist opp-results
      ]
    ]
    if length opp-results != 0[
      
      ;; ENDAST FÖR TEST
      show op_hist
      show own_hist
      show sort modes opp-results
      show "lolerboy returns: "
      show first sort modes opp-results
      ;; //ENDAST FÖR TEST
      
      report first sort modes opp-results
    ]
    ;; ENDAST FÖR TEST
    let theresult (random 9) + 1
    show "lolerboy returns: "
    show theresult
    report theresult
    ;; //ENDAST FÖR TEST
    ;;report (random 9) + 1
  ]
end

;; Median guy    (kör på random tills motståndaren vunnit en gång, tar sedan värdet under motståndarens median)   //Ser ut att fungera (Emil)
to-report median-guy [own_hist op_hist]
  ;; Kolla om listan är tom
  ifelse empty? op_hist[
    report (random 9) + 1
  ][
    ;; Hämta motståndarens värden till en lista
    let results result-list own_hist op_hist length own_hist
    let opp-results []
    foreach (n-values length results [?])[
      if item ? results = -1[
        set opp-results lput item ? op_hist opp-results
      ]
    ]
    if length opp-results != 0[
      report round median opp-results - 1
    ]
    report (random 9) + 1
  ]
end

;; Grudger     (Good guy greg tills motståndaren blåser honom,  sen it’s something guy). 
;; Ser ut att fungera /93
to-report grudger [own_hist op_hist]
  ifelse empty? op_hist[
    report 10
  ][
    let results result-list own_hist op_hist length own_hist
    foreach (n-values length results [?])[
      if item ? results = 0[
        if item ? op_hist = 10[
          report 10
        ]
        report 1
      ]
    ]
    report 1
  ]
end

;; Adjust-guy    (Ökar 1 vid vinst, Minskar 1 vid förlust)  //Borde vara korrekt (Emil)
to-report adjust-guy [own_hist op_hist]
  ifelse empty? op_hist[
    report 5
  ][
    ;; Hämta vem som vann i senaste matchen
    let results result-list own_hist op_hist 1
    if item 0 results = -1[
      if first own_hist > 1[
        report first own_hist - 1
      ]
      report 1
    ]
    if item 0 results = 1[
      if first own_hist < 10[
        report first own_hist + 1
      ]
      report 10
    ]
    report first own_hist
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
812
10
1251
470
16
16
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
208
25
274
58
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
289
25
352
58
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
1

SLIDER
15
23
187
56
num_strategies
num_strategies
1
15
15
1
1
NIL
HORIZONTAL

PLOT
14
69
328
350
Points
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"tit-for-tat" 1.0 0 -8053223 true "" "ask turtle 0[plot points]"
"tit-for-2-tat" 1.0 0 -1184463 true "" "ask turtle 1[plot points]"
"random-dude" 1.0 0 -13345367 true "" "ask turtle 2[plot points]"
"its-something-guy" 1.0 0 -13840069 true "" "ask turtle 3[plot points]"
"scumbag-steve" 1.0 0 -8630108 true "" "ask turtle 4[plot points]"

PLOT
344
70
658
351
Points-2
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"scumbag-stacy" 1.0 0 -8053223 true "" "ask turtle 5[plot points]"
"good-guy-greg" 1.0 0 -1184463 true "" "ask turtle 6[plot points]"
"neil-degrasse-tyson" 1.0 0 -13345367 true "" "ask turtle 7[plot points]"
"robocop" 1.0 0 -13840069 true "" "ask turtle 8[plot points]"
"close-enought-guy" 1.0 0 -8630108 true "" "ask turtle 9[plot points]"

PLOT
676
70
990
351
Points-3
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"even-numbers-guy" 1.0 0 -8053223 true "" "ask turtle 10[plot points]"
"loler-guy" 1.0 0 -1184463 true "" "ask turtle 11[plot points]"
"median-guy" 1.0 0 -13345367 true "" "ask turtle 12[plot points]"
"grudger" 1.0 0 -13840069 true "" "ask turtle 13[plot points]"
"adjust-guy" 1.0 0 -8630108 true "" "ask turtle 14[plot points]"

PLOT
15
360
329
641
num_wins
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"tit-for-tat" 1.0 0 -8053223 true "" "ask turtle 0[plot num_wins]"
"tit-for-2-tat" 1.0 0 -1184463 true "" "ask turtle 1[plot num_wins]"
"random-dude" 1.0 0 -13345367 true "" "ask turtle 2[plot num_wins]"
"its-something-guy" 1.0 0 -13840069 true "" "ask turtle 3[plot num_wins]"
"scumbag-steve" 1.0 0 -8630108 true "" "ask turtle 4[plot num_wins]"

PLOT
345
361
659
642
num_wins-2
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"scumbag-stacy" 1.0 0 -8053223 true "" "ask turtle 5[plot num_wins]"
"good-guy-greg" 1.0 0 -1184463 true "" "ask turtle 6[plot num_wins]"
"neil-degrasse-tyson" 1.0 0 -13345367 true "" "ask turtle 7[plot num_wins]"
"robocop" 1.0 0 -13840069 true "" "ask turtle 8[plot num_wins]"
"close-enought-guy" 1.0 0 -8630108 true "" "ask turtle 9[plot num_wins]"

PLOT
677
361
991
642
num_wins-3
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"even-numbers-guy" 1.0 0 -8053223 true "" "ask turtle 10[plot num_wins]"
"loler-guy" 1.0 0 -1184463 true "" "ask turtle 11[plot num_wins]"
"median-guy" 1.0 0 -13345367 true "" "ask turtle 12[plot num_wins]"
"grudger" 1.0 0 -13840069 true "" "ask turtle 13[plot num_wins]"
"adjust-guy" 1.0 0 -8630108 true "" "ask turtle 14[plot num_wins]"

TEXTBOX
1001
42
1151
86
Plot 1: tit-for-tat \nPlot 2: scumbag-stacy \nPlot 3: even-numbers-guy\n
11
13.0
1

TEXTBOX
1000
92
1150
136
Plot 1: tit-for-2-tat \nPlot 2: good-guy-greg \nPlot 3: loler-guy\n
11
45.0
1

TEXTBOX
999
193
1149
237
Plot 1: its-something-guy \nPlot 2: robocop \nPlot 3: grudger\n
11
65.0
1

TEXTBOX
1000
145
1173
189
Plot 1: random-dude \nPlot 2: neil-degrasse-tyson \nPlot 3: median-guy\n
11
105.0
1

TEXTBOX
999
242
1149
286
Plot 1: scumbag-steve \nPlot 2: close-enought-guy \nPlot 3: adjust-guy\n
11
115.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
