globals [
             filename      ; for output of big-data
             hcount
             data          ; data file for information on individual turtles
             gini-index          ;
             gini_         ; lagged gini
             lorenz-points ;  drawing lorenz curve
             n             ; number of turtles
             n_trans       ; number of transfers
             trans_tot     ; total value of transfers
             p             ; price
             p_            ; lagged price
             sectors
             IPO
             LF
         ]
turtles-own
[
  B                                    ; current budget
  B_                                   ; lagged current budget
  X                                    ; goods
  Y                                    ; future consumption
  alpha                                ; preference for current consumption
  U                                    ; utility
  working?                             ; have a job this sweep-boolean
  purchase?                            ; made purchase this sweep-boolean
  w                                    ; wages
  yrs_wk                               ; experience (years worked)
  wtot                                 ; wage payments total
  fs                                   ; frustrated sales
  tax                                  ; tax collected from rich
  transfers                            ; transfers made to poor
  edu                                  ; education
  age                                  ; age
  ptot                                 ; profit income
  wealth                               ; includes cash and stocks
  wealth_                              ; lagged wealth
  portfolio                            ; stock holdings
  dividends
  share
]

patches-own
[
  Q                                               ; output
  Q_                                              ; lagged output
  beta                                            ; elas of output with respect to capital
  K                                               ; capital stock
  K_                                              ; lagged capital stock
  L                                               ; labor
  MPL                                             ; marginal product of labor
  prof                                            ; residual after paying wages
  inven                                           ; inven
  inven_                                          ; inven lagged
  costs                                           ;
  d_K                                             ; change in capital stock
  inv                                             ; decide to invest or not
]
to setup
  clear-all
  set n 40
  set lorenz-points []
  set sectors
  [
    "Water" "Energy" "Food" "Rest"
  ]
  setup-patches
  setup-turtles
  update-lorenz-and-gini
  set gini_ 0
  set redistribute 0
  reset-ticks
end

to setup-patches
  ask patches
  [
    set pcolor random 1000
    set beta 0.1 + random-float 0.4               ;
    set K 10 + random 50
    set inven 5
    set inven_ 0
    set Q 1
    set Q_ 1
    set p 1
    set p_ 1
    if  K = 0
    [
      set pcolor gray
    ]
  ]

;  assign each patch a sector

  let npatch count patches
  let sectors-list sectors
  repeat npatch
  [
    ask one-of patches with [plabel = ""]
    [
      set plabel item 0 sectors-list
      set sectors-list butfirst sectors-list        ; cycles through sector list
    ]
  ]



end

to setup-turtles
  create-turtles  n
  [
   set alpha (0.1  +  random-float 0.6)
   set B 1 + random 5
   setxy random-xcor random-ycor
   set U 1
   set shape "circle 2"
   set size 0.1
   set age 15 + random 60
   set edu 8 + random 12
   set portfolio []
  ]

  ; distribute shares of capital stock  to turtles

  let sectors-list sectors

  repeat length sectors
  [
    let sector-tmp  one-of patches with [plabel = item 0 sectors-list]
    let k_tmp [K] of sector-tmp + [inven] of sector-tmp
    ;type " sector = " print sector-tmp
    ;type " k_tmp = " print k_tmp
    ask turtles
    [
      set share random-float 1
    ]

    let tmp sum [share] of turtles
    ask turtles
    [
      set share share / tmp * k_tmp
      ;type "share = " print share
    ]

      ask turtles
      [
        let  share-of-sector item 0 sectors-list
        ;type "share-of-sector = " print share-of-sector
        set portfolio lput list share-of-sector share portfolio
      ]
      set sectors-list butfirst sectors-list
    ]


    ask turtles
    [
      ;type " shares " print portfolio
    ]

    let j  0

    ask turtles
    [

    repeat length sectors
      [
        set wealth wealth + last item j portfolio
        set j j + 1
      ]

    ;type "wealth of turtle " type who type " is " print wealth
    set j 0
    ]

 ;  see monitor for wealth error
end




to go
  ;if  (abs ( gini-index - gini_)  < 0.005  ) [ stop ]
  if ticks > 199 [stop]
  ;update-price           [portfolio] of turtle 0                                 ; change price based on inventory adjustment
  save-lagged                                             ; current data as lagged before the rest of go changes it
  look-for-job                                            ; turtle procedure
  produce                                                 ; patch procedure
  demand                                                  ; turtle procedure
  update-budget                                           ; get ready for next round
  if redistribute = 1
  [
    help-poor
  ]
  decide-to-invest
  invest
  distribute-capital-gains
  update-lorenz-and-gini

;  big-data
  tick
end

  to  save-lagged
  ask patches
  [
    set d_K 0
    set inven_ inven
    set p_ p
    set Q_ Q
    set gini_ gini-index
    set K_ K

  ]
  ask turtles
  [
    set Wealth_ Wealth
    set B_ B
    set working? false
    set purchase?  false
    set dividends 0

   ]


  end

to look-for-job
 ask turtles
  [
    right random 360
    forward 3
    if [k] of patch-here > 0
    [
      set working? TRUE
      set yrs_wk yrs_wk + 1
    ]
  ]
end
to produce                                                    ; patch procedure
  ask patches
  [
    set L count turtles-here                                  ; each turtle supplies one unit of labor
    set Q  K ^ beta * L ^ (1 - beta)                          ; production function
    set inven  inven_ + Q                                     ; add Q to inventory
    ifelse any? turtles-here
    [
      set MPL  ( 1 - beta ) * Q / L

    ] ; determine marginal product if producing
    [
      set MPL 0
      set Q 0
    ]                                                         ; if not then set to zero
    ask turtles-here
    [
      set w p *  MPL
    ]                                                         ; this is a nominal wage based on the last price paid
  ifelse Q > 0
   [
      set costs  p * MPL * L                                  ; costs is the wage bill since there are no itermediate
      set prof p * Q - costs                                  ; profits is revenue minus costs
      ;set pcolor green + 3
   ]
   [
      set costs 0
      set prof 0
      ;set pcolor blue  + 3
    ]
    if K = 0
    [
      set pcolor gray
    ]
    ]
end


to demand                                                   ; since more than one turtle can arrive on the same patch
                                                            ;
ask turtles                                                 ; turtle
  [
    right random 360                                        ; turtle arrives on new patch--may be the second to arrive
    forward 3
    set X alpha * B / p                                     ; sets demand according to price on patch--this can vary
    let Qd X                                                ; local variable how much turtles wants to spend
    ifelse Qd <= inven                                      ; is demand less than or equal to what patch is willing to sell?
    [
      set purchase? true                                    ; turtle makes purchase
      set inven inven - Qd                                  ; NB:  patch data adjusted to reduce inventories.
      set  Y  (1 - alpha) * B                               ; compute Y
      set  U X ^ alpha * Y ^ ( 1 - alpha )                  ; utility

    ]                                                       ; If inventory is insufficient
    [
      set X 0
      set fs fs + 1                                         ; increase the counter for frustrated sales by one
    ]
  ]
end

  to update-budget                                          ; turtle procedure
 ;  first compute share of dividends for each turtle
  let sectors-list sectors
  repeat length sectors
  [
    let jj 0
    let div 0
    let dividend-payout 0
    let portfolio-total 0
    let sector-name ""


      set jj 0
      set sector-name item jj sectors-list
       ;type " sector name -> " print sector-name
       let prof_  [prof] of one-of patches with [plabel = sector-name]
       ;type " prof update-budget " print prof_
    ask turtles
    [
    set portfolio-total portfolio-total + last item jj portfolio

    ]
    ;type " portfolio total update-budget " print portfolio-total

    ask turtles
    [
      set div last item jj portfolio / portfolio-total  * prof_
      set dividend-payout dividend-payout + div
      set dividends dividends + div
    ]
    let error-dividends  dividend-payout - prof_
    ;type " dividends error " print error-dividends
    set sectors-list butfirst sectors-list
    set portfolio-total 0
    set dividend-payout 0
    set jj jj + 1

  ]






  ask turtles
  [
        if purchase? = true
        [
          set B B - p * X
        ]                                                   ;  what you didn't spend goes into next period's savings
        ifelse working? = true
    [
      set B B + w  + dividends

      set wtot wtot + w
      set wealth wealth + B - B_
      set color blue

    ]

    [
      set color red
    ]

  ]
  end

  to help-poor                                              ; turtle  procedure
  ask turtles
  [
    set transfers 0
  ]
  let n_rich 0.2 * n
  let rich max-n-of n_rich turtles [B]  ; take top twenty percent of turtles and define them as rich
  ask rich
    [                                                       ;type "1. rich is " print who
      let Br B                                              ; Br is the budget of the rich
      let trans 0
      if any? other turtles-on neighbors
      [
        let poor nobody                                     ; erase previous poor definition
        let near_turtles other turtles-on neighbors         ; pick a cohort of turtles that might need help
        let Bm min [B] of near_turtles                      ; look at the minimum B they have
        set poor one-of near_turtles with [B = Bm]          ; pick the poorest one
        ask poor                                            ; make the transfer to the poor
       [
          ifelse (B < .02 * Br)                             ; is the recpient really poor
          [
            set trans .05 * Br                              ; transfer five percent of rich income
                                                            ; type "2.  poor B = "  print B
            set B B + trans
            set transfers transfers + trans                 ; sum current transfers
                                                            ; type "3.  poor B is now " print B
            set n_trans n_trans + 1                         ; tranfer counter
            set trans_tot trans_tot + trans                 ; total value of transfers
          ]                                                 ; type "4. trans " print n_trans ;
                                                            ; type "5. turtle " type who type " got " print tran    ]
          [
            set trans 0
          ]
        ]
                                                            ; type "6. rich " type who type " gave " print trans
                                                            ;type "7. budget was " print B
        set B  B - trans
        set tax tax + trans                                 ; adjust rich budget
                                                            ;type "9.  budget is " print B
                                                            ;print " " print " "
      ]
  ]
end

  to update-price                                           ; patch procedure
    ask patches
  [                                                         ; increase or decrease price
    if Q != 0
    [
      ifelse inven / Q  > inven_ / Q_
      [
        set p p * 0.98
      ]
      [
        set p p * 1.02
      ]
    ]
  ]
  end

;  invest in new capital stock  (using the El Farol model as a guide for which patch invests and which patch does not invest



to decide-to-invest



ask patches
    [
      set inv 0                   ;  nobody is investing
      if random-float 1 < 0.5
      [
        set inv 0
      ]

      ask patch 0 0               ; only tesla is investing
        [ set inv 1]
  ]
end


to distribute-capital-gains
  ;print "--> now distribute capital gains"
  let jj 0
  repeat length sectors
  [
    let current-portfolio-value 0
    ask turtles
      [ set current-portfolio-value   current-portfolio-value  + item 1 item jj portfolio]
    ;type "current-portfolio-value = " type current-portfolio-value type " for " print item jj sectors
    ;type "current sector-value = " print sector-value (item jj sectors)
    let sector position jj sectors

  ; now update turtles portfolio valus so that they are consistent with the value of the sector
    let capital-gains-ratio sector-value item jj sectors / current-portfolio-value
    ;type "capital gains ratio = "  print capital-gains-ratio
    ask turtles
    [
      ;type "jj = " type jj type " for turtle "  print who
      let share-value last item jj portfolio
      ;type " share-value is " print last item jj portfolio
      let new-share-value share-value * capital-gains-ratio
      ;type " new share-value is " print new-share-value
      let sub-list item jj portfolio
      set portfolio replace-subitem jj 1 portfolio new-share-value
    ]
    set jj jj + 1
  ]
end

to-report replace-subitem [index1 index2 lists value]
  let old-sublist item index1 lists
  report replace-item index1 lists (replace-item index2 old-sublist value)
end

to invest  ;
  let invest_err 0
  let beginning-inven  sum [inven] of patches
  if count patches with [inv > 0] > 0

  [let investor  one-of patches with [inv > 0 ]

    ;type " investor is "  print [plabel] of investor
    ;type " total inventories are " print beginning-inven
    ask investor
    [
      set d_K  0.1 * K                                        ;  ad hoc investment--simplicity is only virtue
      set invest_err d_K + sum [inven] of patches
      while  [d_k > 0]                                        ;  d_k needs supplies from suppliers--add to aggregate demand.
      [
        let supplier one-of other patches with [inven > 0]    ;  find at least one supplier


        ifelse (supplier = nobody)                            ;
        [
          set d_K  0                                         ;  no investment
        ]
        [
          ;type " supplier has inventory is "  print [plabel] of supplier
          ifelse  d_k < [inven] of supplier
            [
              ;type  "supplier inventory adequate = " print [inven] of supplier
              ;type  "investor inventory  = " print [inven] of investor
              ;type " investor needs  " print d_K
              ;type " adjusting investor capital stock  from " type K

              set K K + d_k
              ;type " to " print K
              ask supplier
              [
                ;type "adjusting supplier inventory by " print [d_k] of investor
                set inven inven - [d_k] of investor

              ]
              set d_K 0
              ;type " supplier inventories now  " print  [inven] of supplier
              ;type " total inventories now  " print  sum [inven] of patches

          ]

          [
            ;print "supplier inventory NOT adequate"
            set K K + [inven] of supplier
            set d_K d_K - [inven] of supplier

            ask supplier
            [
              set inven 0
            ]
          ]
        ]
      ]
      ;set sd skl_lab K                                       ; upgrade skill demand
    ]
    ;type " invest error " print (sum [k] of patches - sum [k_] of patches  + (sum [inven] of patches - beginning-inven ))
  ]

end



to-report wealth-sum [portfolio_]
  let wealth-sum_ 0
  let j 0
  repeat length sectors
      [
        set wealth-sum_  wealth-sum_ + last item j portfolio_
        set j j + 1
      ]
report wealth-sum_
end


to-report wealth-err
  ;  compute wealth

  let wealth_error_  sum [wealth] of turtles - sum[K] of patches  - sum[inven] of patches
  report wealth_error_

end


to-report sector-value [sector-name]
  let sector-value_ 0
  let sector one-of patches with [plabel = sector-name]
  set sector-value_
  [k] of sector
    + [inven] of sector
   report sector-value_
end

to-report share-err [sector-name]
      let share-err_ 0
      let jj position sector-name sectors
  set share-err_  sum [last item jj portfolio] of turtles - sector-value sector-name
  report share-err_
    end


;  make SAM

  to-report C   ; total consumption
    let C_ sum [ p * X ] of turtles with [purchase? = TRUE]
    report C_
  end

  to-report I   ; total investment
    let I_  sum [p * (inven - inven_ + K - K_) ] of patches
    report I_
  end

  to-report VA    ; value added
    let VA_ sum [w] of turtles with [working? = TRUE] + sum [prof] of patches
    report  VA_
  end

   to-report Wages    ; value added
    let Wages_ sum [w] of turtles with [working? = TRUE]
    report  Wages_
  end

  to-report Profits    ; value added
    let Profits_  sum [prof] of patches
    report  Profits_
  end

  to-report Yh   ; household income
    let Yh_ sum [w] of turtles with [working? = TRUE] + sum [prof] of patches
    report  Yh_
  end

  to-report S     ; household savings
     let S_   sum [w] of turtles with [working? = TRUE] + sum [prof] of patches
            - sum [ p * X ] of turtles with [purchase? = TRUE]
     report  S_
  end

  to-report GDP
    report  sum [p * Q] of patches  ;
  end

  to-report SI-err
    let I_ sum [p * (inven - inven_ + K - K_ ) ] of patches
    let delta_wealth  sum [Wealth] of turtles - sum[Wealth_] of turtles
    report  delta_wealth - I_ ; total savings of turtles minus investment
  end

to big-data
set filename "data.csv"
;  if hcount = 0
; [write-csv filename (list "who" "alpha" "B" "yrs_wk" "wtot" "ptot" "fs" "tax" "transfers" "time" "gini")]
ask turtles [ write-csv filename (list who alpha b yrs_wk wtot ptot fs tax transfers ticks gini-index redistribute) ]
set hcount hcount + 1
end

to write-csv [ #filename #items ]  ;; #items is a list of the data (or headers!) to write.
 if is-list? #items and not empty? #items
 [ file-open #filename
 ;; quote non-numeric items
 set #items map quote #items
 ;; print the items
 ;; if only one item, print it.
 ifelse length #items = 1 [ file-print first #items ]
 [file-print reduce [ [?1 ?2] -> (word ?1 "," ?2) ] #items]
 ;; close-up
 file-close
 ]
 end


 to-report quote [ #thing ]
 ifelse is-number? #thing
 [ report #thing ]
 [ report (word "\"" #thing "\"") ]
 end




 to-report te_rule
   let n_rich 0.2 * n
   let B_tot sum [B] of turtles
   let B_rich sum [B] of max-n-of n_rich turtles [B]
   let te_rule_ B_rich / B_tot
   report te_rule_
 end

;; this procedure recomputes the value of gini-reserve
;; and the points in lorenz-points for the Lorenz and gini plots


to update-lorenz-and-gini
  let sorted-wealths sort [B] of turtles
  let total-wealth sum sorted-wealths
  let wealth-sum-so-far 0
  let index 0
  set gini-index 0
  set lorenz-points []   ;  a list
  set lorenz-points fput 0 lorenz-points

  ;; now actually plot the Lorenz curve -- along the way, we also
  ;; calculate the Gini index.

  repeat n  [
    set wealth-sum-so-far (wealth-sum-so-far + item index sorted-wealths)
    set lorenz-points lput ((wealth-sum-so-far / total-wealth) * 100) lorenz-points
    set index (index + 1)
    set gini-index
    gini-index +
    (index / n) -
    (wealth-sum-so-far / total-wealth)
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
240
10
648
419
-1
-1
200.0
1
10
1
1
1
0
1
1
1
0
1
0
1
1
1
1
ticks
30.0

BUTTON
39
40
123
73
NIL
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
126
40
203
73
NIL
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

MONITOR
60
170
149
215
Working
count turtles with [working? = true]
3
1
11

MONITOR
60
215
157
260
Producing firms
count patches with [Q > 0]
0
1
11

PLOT
19
291
219
441
Totals
time
totals
0.0
5.0
0.0
5.0
true
true
"" ""
PENS
"GDP" 1.0 0 -16777216 true "" "if sum [Q] of patches > 0 [plot ln sum [Q] of patches]"
"U" 1.0 0 -10899396 true "" "if sum [U] of turtles > 0 [plot ln sum [U] of turtles]"
"p" 1.0 0 -8053223 true "" "plot mean [p] of patches"

BUTTON
132
79
195
112
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
779
402
836
447
NIL
SI-err
2
1
11

MONITOR
774
136
831
181
C
C
2
1
11

MONITOR
828
136
885
181
I
I
2
1
11

MONITOR
885
136
942
181
GVP
GDP
2
1
11

MONITOR
717
183
774
228
VA
VA
2
1
11

MONITOR
720
359
777
404
GVP
GDP
2
1
11

MONITOR
885
180
949
225
Yh_total
Yh
2
1
11

MONITOR
778
359
835
404
Yh
Yh
2
1
11

MONITOR
778
313
835
358
S
S
2
1
11

MONITOR
886
314
943
359
S_total
S
2
1
11

MONITOR
834
359
891
404
I
I
2
1
11

TEXTBOX
761
93
911
111
Social Accounting Matrix
11
0.0
1

PLOT
835
403
1035
553
Wealth Distribution
NIL
NIL
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"Wealth" 1.0 1 -16777216 true "" "set-plot-x-range 0 max [B] of turtles\nset-plot-y-range 0 count turtles\nset-histogram-num-bars 20\nhistogram [B] of turtles"

MONITOR
779
447
836
492
NIL
te_rule
2
1
11

MONITOR
717
226
774
271
Wages
Wages
2
1
11

MONITOR
885
225
942
270
Wages
Wages
2
1
11

MONITOR
717
271
774
316
Profits
Profits
2
1
11

MONITOR
886
269
943
314
Profits
Profits
2
1
11

MONITOR
15
515
105
560
GDP deflator
sum [p * Q] of patches / sum [Q] of patches
5
1
11

PLOT
971
225
1171
375
Gini
NIL
NIL
0.0
50.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (gini-index / n) / 0.5"

PLOT
969
75
1169
225
Lorenz curve
NIL
NIL
0.0
100.0
0.0
100.0
false
false
"" "plot-pen-reset\nset-plot-pen-interval 100 / n\nforeach lorenz-points plot"
PENS
"lorenz" 1.0 0 -2674135 true "" "plot-pen-reset\nset-plot-pen-interval 100 / n\nforeach lorenz-points plot"
"equal" 100.0 0 -16777216 true "plot 0\nplot 100" ""

MONITOR
61
125
148
170
Households
n
17
1
11

MONITOR
108
515
181
560
Transfers
trans_tot
1
1
11

MONITOR
181
515
238
560
Tax
sum [tax] of turtles
17
1
11

MONITOR
1077
385
1134
430
Gini
(gini-index / n) / 0.5
17
1
11

SLIDER
641
498
813
531
redistribute
redistribute
0
1
0.0
1
1
NIL
HORIZONTAL

MONITOR
240
515
297
560
delta K
sum[K] of patches - sum [K_] of patches
3
1
11

MONITOR
297
515
354
560
Inv
sum [d_K] of patches
17
1
11

MONITOR
487
430
576
475
Wealth error
wealth-err
2
1
11

MONITOR
241
431
298
476
Water
sector-value \"Water\"
2
1
11

MONITOR
240
473
297
518
Water
Share-err \"Water\"
2
1
11

MONITOR
294
431
351
476
Food
sector-value \"Food\"
2
1
11

TEXTBOX
139
450
246
468
Company values
11
0.0
1

TEXTBOX
154
476
226
494
Share error
11
0.0
1

MONITOR
293
475
350
520
Food
Share-err \"Food\"
2
1
11

MONITOR
345
432
414
477
Energy
sector-value \"Energy\"
2
1
11

MONITOR
346
475
415
520
Energy
Share-err \"Energy\"
2
1
11

MONITOR
415
432
474
477
Rest
sector-value \"Rest\"
2
1
11

MONITOR
415
476
472
521
Rest
share-err \"Rest\"
2
1
11

@#$#@#$#@
## WHAT IS IT?

The construction of this model is described in Tutorial 3 in the NetLogo User Manual.
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
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Redistribution without growth" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>GDP</metric>
    <metric>median [B] of turtles</metric>
    <metric>sum [transfers] of turtles</metric>
    <metric>gini-index</metric>
    <metric>mean [K] of patches</metric>
    <enumeratedValueSet variable="redistribute">
      <value value="0"/>
      <value value="1"/>
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
