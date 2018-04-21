'erl       
'1-25-99, 3-31-99   
'Rocket program      
DECLARE SUB PsetStars2 ()
DECLARE SUB PsetStars1 ()
DECLARE SUB Intro ()
DECLARE SUB Ending ()
DECLARE SUB RocketMelt ()
DECLARE SUB PlanetBG ()
DECLARE SUB MeltRay ()
DECLARE SUB RocketLand ()
DECLARE SUB ApproachPlanet ()
DECLARE SUB Fly2Planet (M)
DECLARE SUB Stars2 ()
DECLARE SUB Stars1 ()
DECLARE SUB Fire2 (Y)
DECLARE SUB Fly1 (M)
DECLARE SUB Warp ()
DECLARE SUB Countdown ()
DECLARE SUB Launch ()
DECLARE SUB Fire1 (X)
DECLARE SUB DoorOpen ()
DECLARE SUB DoorClose ()
DECLARE SUB LaunchBackGround ()
DECLARE SUB GrassMove ()
CLS
SCREEN 12      '640x480
RANDOMIZE TIMER

CALL Intro
CALL Stars1
CALL LaunchBackGround
CALL GrassMove
CALL DoorOpen
CALL Countdown
CALL Launch
CALL DoorClose
CALL Warp
FOR Flies = 1 TO 3
   CALL Fly1(M)
NEXT Flies
CALL Fly2Planet(M)
CALL ApproachPlanet
CALL PlanetBG
CALL RocketLand
CALL MeltRay
CALL RocketMelt
CALL Ending

COLOR 0
END

'Data for song
DATA 221, 1, 32767, 2, 147, 1, 32767, 5, 294, 3, 294, 3, 278, 3, 248, 3, 278, 3, 32767, 3, 294, 3, 330, 3, 32767, 3, 221, 1, 32767, 2, 147, 1, 32767, 5, 294, 3, 294, 3, 278, 3, 248, 3, 278, 3, 32767, 3, 294, 3, 330, 3
DATA 32767, 3, 221, 3, 294, 3, 32767, 3, 350, 3, 441, 6, 393, 3, 350, 3, 32767, 3, 441, 3, 525, 6, 467, 3, 441, 3, 32767, 3, 556, 3, 661, 6, 589, 3, 556, 3, 467, 3, 441, 3, 393, 3, 350, 3, 330, 3, 147, 1, 32767, 5, 294, 3, 294, 3, 278, 3, 248, 3, 278, 3
DATA 32767, 3, 294, 3, 330, 3, 32767, 3, 221, 1, 32767, 2, 147, 1, 32767, 5, 294, 3, 294, 3, 278, 3, 248, 3, 278, 3, 32767, 3, 294, 3, 330, 3, 32767, 3, 221, 3, 350, 3, 32767, 3, 441, 3, 525, 6, 467, 3, 441, 3, 393, 3, 350, 3, 312, 3, 393, 3, 467, 3, 294, 3, 278, 3, 294, 3, 330, 3, 32767, 3, 350, 1, 32767, 2, 330, 9, 294, 1

SUB ApproachPlanet
CLS

CIRCLE (320, 101), 2, 15    'planet
CIRCLE (320, 101), 1, 3     'planet

FOR Star = 1 TO 200                         'pset stars
   LET C = C + 1: IF C = 14 THEN LET C = 1
   PSET (RND * 640, RND * 480), C
NEXT Star
FOR Star = 1 TO 40                         'other * stars
   LET C = C + 1: IF C = 14 THEN LET C = 1: IF C = 3 THEN LET C = C + 1
   COLOR C
   LOCATE INT(RND * 28) + 1, INT(RND * 80) + 1
   PRINT "*"
NEXT Star

LINE (0, 0)-(250, 300), 14             'rocket inside view
LINE (250, 300)-(390, 300), 14
LINE (390, 300)-(640, 0), 14
LINE (250, 300)-(0, 400), 14
LINE (390, 300)-(640, 400), 14
PAINT (320, 470), 7, 14
PAINT (0, 200), 8, 14
PAINT (639, 200), 8, 14

CIRCLE (330, 422), 20, 8            'control panel & buttons
PAINT STEP(0, 0), 4, 8               ' the red button
CIRCLE (290, 360), 15, 10           'green buttons
PAINT STEP(0, 0), 15, 10
CIRCLE (330, 360), 20, 10
PAINT STEP(0, 0), 15, 10
CIRCLE (380, 360), 25, 10
PAINT STEP(0, 0), 15, 10
CIRCLE (440, 360), 30, 10
PAINT STEP(0, 0), 15, 10
LINE (160, 350)-(200, 460), 13, BF    'altimeter
LINE (220, 350)-(260, 460), 9, BF     'bluemeter
LINE (20, 420)-(120, 460), 12, BF     'redmeter
LINE (180, 355)-(180, 455), 0          'line in mid of altimeter
LINE (25, 440)-(115, 440), 15          'line in middle of redmeter
LINE (240, 355)-(240, 455), 0          'line in mid of bluemeter

LOCATE 26, 45: PRINT "                              "
LOCATE 28, 45: PRINT "                              "
LOCATE 27, 45: COLOR 10
PRINT "  Scanning planet surface.    "
FOR Del = 1 TO 10000: NEXT Del

LOCATE 27, 45: COLOR 10
PRINT "   Approaching the planet.    "

FOR Rad = 0 TO 175 STEP .1
   IF Rad = 0 THEN PAINT (290, 360), 2, 10
   IF Rad > 45 THEN PAINT (330, 360), 2, 10
   IF Rad > 110 THEN PAINT (380, 360), 2, 10
   IF Rad > 160 THEN PAINT (440, 360), 2, 10
   A1 = 455 - (Rad / 1.83)         'altimeter line
   LINE (165, A1)-(195, A1), 14
   A2 = 115 - (Rad / 1.96)        'red meter line
   LINE (A2, 425)-(A2, 455), 10
   A3 = 355 + (Rad / 1.83)        'bluemeter line
   LINE (225, 405)-(255, A3), 12, BF
   LINE (180, 355)-(180, 455), 0          'line in mid of altimeter
   LINE (25, 440)-(115, 440), 15          'line in middle of redmeter
   LINE (240, 355)-(240, 455), 0          'line in mid of randmeter
   CIRCLE (320, 101), Rad + 2, 15   'planet
   CIRCLE (320, 101), Rad + 1, 3    'planet
   CIRCLE (320, 100), Rad, 3        'planet
   IF Rad > 100 AND Rad < 101 THEN
      LOCATE 27, 45: COLOR 10
      PRINT "       Prepare to land.      "
   END IF
   IF Rad > 150 THEN
      LOCATE 27, 45: COLOR 10
      PRINT " Landing sequence initiated.  "
   END IF
   'erasings
   LINE (165, A1 + 1)-(195, A1 + 1), 13       'erase altmeter
   LINE (A2 + 1, 425)-(A2 + 1, 455), 12       'erase red meter
   LINE (225, A3)-(255, A3), 9                'erase bluemeter
NEXT Rad
FOR Del = 1 TO 3000: NEXT Del

'clear screen
FOR X = 0 TO 320 STEP 5
   LINE (320 - X, 240 - X)-(320 + X, 240 + X), 0, BF
NEXT X

END SUB

SUB Countdown

LINE (0, 130)-(180, 200), 0, BF
LOCATE 10, 2: COLOR 10
PRINT "Initiating Launch"

FOR Time = 10 TO 0 STEP -1
   SOUND 1000 + (Time * 100), 18.2
   LOCATE 11, 2: PRINT "           "
   LOCATE 11, 2: COLOR 12
   PRINT "    T - "; Time
   FOR Del = 1 TO 1000: NEXT Del
   IF Time = 9 THEN
      LOCATE 10, 2: PRINT "                 "
      LOCATE 10, 2: COLOR 10
      PRINT " Counting Down"
   END IF
NEXT Time

END SUB

SUB DoorClose

LINE (0, 130)-(180, 200), 0, BF
LOCATE 10, 2: COLOR 10
PRINT "Closing Launch"
LOCATE 11, 2
PRINT "Door"

FOR Door = 150 TO 1 STEP -1
   SOUND (200 * RND) + (200 * RND) + 37, .03
   FOR Delay = 1 TO 100: NEXT Delay
   LINE (170, 300)-(320 - Door, 480), 7, BF
   LINE (470, 300)-(320 + Door, 480), 7, BF
NEXT Door

'clear screen curtains
FOR Z = 4 TO 2 STEP -2
   FOR X = 0 TO 320 STEP Z
      LINE (X, 1)-(X, 480), 0   'left
      LINE (640 - X, 1)-(640 - X, 480), 0 'right
   NEXT X
NEXT Z
END SUB

SUB DoorOpen

LINE (0, 130)-(180, 200), 0, BF
LOCATE 10, 2: COLOR 10
PRINT "Opening Rocket"
LOCATE 11, 2
PRINT "Launch Door"

FOR Door = 1 TO 150
   FOR Delay = 1 TO 500: NEXT Delay
   LINE (320 - Door, 300)-(320 + Door, 480), 0, BF
NEXT Door

END SUB

SUB Ending
SCREEN 9

'Data for end screen
Text$ = "About this rocket"
T1$ = "This rocket was created by Eric Laslo"
T2$ = "I learned how to use SUBs to make programming much more effective."
T3$ = "I also learned that using delays to control the speed of the program makes it   look a lot better."
T4$ = "I found out that a lot of the things that you try don't work out the way you    want them to and you must change them."
T5$ = "I also learned that it is very easy to make things clear the screen better than CLS and that makes the program look a lot better."
T6$ = "The End"

'scrolls text up the screen
FOR X = 22 TO 1 STEP -1
   CLS
   COLOR 14
   LOCATE X, 40 - (.5 * LEN(Text$)): PRINT Text$
   FOR Del = 1 TO 5000: NEXT Del
NEXT X

'Text being typed out
COLOR 15

PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T1$)
      PRINT MID$(T1$, Z, 1);
      SOUND 100, .1
      FOR Del = 1 TO RND * 400: NEXT Del
NEXT Z

PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T2$)
      PRINT MID$(T2$, Z, 1);
      SOUND 100, .1
      FOR Del = 1 TO RND * 400: NEXT Del
NEXT Z

PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T3$)
      PRINT MID$(T3$, Z, 1);
      SOUND 100, .1
      FOR Del = 1 TO RND * 400: NEXT Del
NEXT Z

PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T4$)
      PRINT MID$(T4$, Z, 1);
      SOUND 100, .1
      FOR Del = 1 TO RND * 400: NEXT Del
NEXT Z

PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T5$)
      PRINT MID$(T5$, Z, 1);
      SOUND 100, .1
      FOR Del = 1 TO RND * 400: NEXT Del
NEXT Z

FOR X = 22 TO 17 STEP -1
   COLOR 12
   LOCATE X, 40 - (.5 * LEN(T6$)): PRINT T6$
   LOCATE X + 1, 40 - (.5 * LEN(T6$)): PRINT "            "
   FOR Del = 1 TO 15000: NEXT Del
NEXT X

END SUB

SUB Fire1 (X)

FOR fire = 1 TO 3
   LET YR = RND * (60 - fire)
   CIRCLE (320 - YR, X + 410), fire + 15, 4
   CIRCLE (320 + YR, X + 410), fire + 15, 4
   'smoke -NOT used
   'CIRCLE (320 - YR, X + 440), fire + 16, 8
   'CIRCLE (320 + YR, X + 440), fire + 16, 8
NEXT fire
LINE (240, X + 450)-(400, X + 490), 0, BF 'erases fire

END SUB

SUB Fire2 (Y)

FOR fire = 1 TO 3
   LET XR = RND * (30 - fire)
   CIRCLE (Y + 195, 240 - XR), fire + 5, 4
   CIRCLE (Y + 195, 240 + XR), fire + 5, 4
   'smoke - not used
   'CIRCLE (Y + 215, 240 - XR), fire + 6, 8
   'CIRCLE (Y + 215, 240 + XR), fire + 6, 8
NEXT fire
LINE (Y + 230, 200)-(Y + 230, X + 280), 0, BF 'erases fire

END SUB

SUB Fly1 (M)
CLS

CALL Stars2

M1$ = "T120 ML L16 o2 b a g# a > c4 d c < b >  c e4 f e d# e b a g# a b a g# a > c4"
M2$ = "L8  o3 a b > c < b a g# a e f d c4 < b4 a4 a b > c#4 < a b > c# < b a g# f# g# a b g# e"
M3$ = "o3 a b > c#4 < a b > c# < b a g# f# b g# e a. p16 a b > c#4 < a b > c# < b"
M4$ = "a g# f# g# a b g# e  a b > c#4 < a b > c# < b a g# f# b g# e a4"
PLAY ON
PLAY "mb"

FOR Y = 640 TO -300 STEP -1
   SELECT CASE PLAY(void)
     CASE 0
     IF M = 1 THEN PLAY M1$
         IF M = 2 THEN PLAY M2$
         IF M = 3 THEN PLAY M3$
         IF M = 4 THEN PLAY M4$
         M = M + 1: IF M = 5 THEN M = 1
   END SELECT
   LET C = C + 1: IF C = 16 THEN LET C = 1
   LINE (Y, 240)-(Y + 30, 220), C        'nose up
   LINE (Y, 240)-(Y + 30, 260), C       'nose bottom
   LINE (Y + 30, 220)-(Y + 30, 260), C 'nose back
   LINE (Y + 30, 220)-(Y + 80, 220), C   'upper body line
   LINE (Y + 30, 260)-(Y + 80, 260), C   'lower body line
   LINE (Y + 180, 215)-(Y + 180, 265), C 'bottom
   LINE (Y + 80, 220)-(Y + 205, 200), C  'top body fin
   LINE (Y + 80, 260)-(Y + 205, 280), C  'bottom body fin
   LINE (Y + 205, 200)-(Y + 180, 215), C 'top back interior
   LINE (Y + 205, 280)-(Y + 180, 265), C 'bottom back interior
   LINE (Y + 45, 220)-(Y + 120, 195), C      'top front fin
   LINE (Y + 120, 195)-(Y + 80, 220), C
   LINE (Y + 45, 260)-(Y + 120, 285), C      'bottom front fin
   LINE (Y + 120, 285)-(Y + 80, 260), C
   CALL Fire2(Y)
   'erasing lines
   IF Y / 5 = INT(Y / 5) THEN CALL PsetStars2
   LINE (Y + 180, 215)-(Y + 180, 265), 0   'bottom
   LINE (Y + 205, 200)-(Y + 180, 215), 0   'top back interior
   LINE (Y + 205, 280)-(Y + 180, 265), 0   'bottom back interior
   LINE (Y + 120, 195)-(Y + 80, 220), 0    'top front fin
   LINE (Y + 120, 285)-(Y + 80, 260), 0    'bottom front fin
NEXT Y

END SUB

SUB Fly2Planet (M)

CLS
CALL Stars2

LOCATE 22, 29: COLOR 10
PRINT "Planet is in view."

CIRCLE (50, 50), 50, 15
PAINT STEP(0, 0), 3, 15

M1$ = "T120 ML L16 o2 b a g# a > c4 d c < b >  c e4 f e d# e b a g# a b a g# a > c4"
M2$ = "L8  o3 a b > c < b a g# a e f d c4 < b4 a4 a b > c#4 < a b > c# < b a g# f# g# a b g# e"
M3$ = "o3 a b > c#4 < a b > c# < b a g# f# b g# e a. p16 a b > c#4 < a b > c# < b"
M4$ = "a g# f# g# a b g# e  a b > c#4 < a b > c# < b a g# f# b g# e a4"

FOR Y = 640 TO 150 STEP -1
   SELECT CASE PLAY(void)
      CASE 0
         IF M = 1 THEN PLAY M1$
         IF M = 2 THEN PLAY M2$
         IF M = 3 THEN PLAY M3$
         IF M = 4 THEN PLAY M4$
         M = M + 1: IF M = 5 THEN M = 1
   END SELECT
   LET C = C + 1: IF C = 16 THEN LET C = 1
   LINE (Y, 240)-(Y + 30, 220), C        'nose up
   LINE (Y, 240)-(Y + 30, 260), C       'nose bottom
   LINE (Y + 30, 220)-(Y + 30, 260), C 'nose back
   LINE (Y + 30, 220)-(Y + 80, 220), C   'upper body line
   LINE (Y + 30, 260)-(Y + 80, 260), C   'lower body line
   LINE (Y + 180, 215)-(Y + 180, 265), C 'bottom
   LINE (Y + 80, 220)-(Y + 205, 200), C  'top body fin
   LINE (Y + 80, 260)-(Y + 205, 280), C  'bottom body fin
   LINE (Y + 205, 200)-(Y + 180, 215), C 'top back interior
   LINE (Y + 205, 280)-(Y + 180, 265), C 'bottom back interior
   LINE (Y + 45, 220)-(Y + 120, 195), C      'top front fin
   LINE (Y + 120, 195)-(Y + 80, 220), C
   LINE (Y + 45, 260)-(Y + 120, 285), C      'bottom front fin
   LINE (Y + 120, 285)-(Y + 80, 260), C
   CALL Fire2(Y)
   'erasing lines
   IF Y / 5 = INT(Y / 5) THEN CALL PsetStars2
   LINE (Y + 180, 215)-(Y + 180, 265), 0   'bottom
   LINE (Y + 205, 200)-(Y + 180, 215), 0   'top back interior
   LINE (Y + 205, 280)-(Y + 180, 265), 0   'bottom back interior
   LINE (Y + 120, 195)-(Y + 80, 220), 0    'top front fin
   LINE (Y + 120, 285)-(Y + 80, 260), 0    'bottom front fin
NEXT Y

END SUB

SUB GrassMove

LINE (0, 130)-(180, 200), 0, BF
LOCATE 10, 2: COLOR 10
PRINT "Opening Secret"
LOCATE 11, 2
PRINT "Launch Area"

FOR Grass = 300 TO 480
   FOR Delay = 1 TO 400: NEXT Delay
   LINE (170, Grass)-(470, Grass), 7      'rocket bay
   LINE (320, Grass)-(320, Grass), 0
NEXT Grass

END SUB

SUB Intro

'Data for text
T1$ = "erl"
T2$ = "Rocket Program"
T3$ = "3.31.99" 'version 9
T4$ = "Background: A new planet has been discoved in the outreaches of the galaxy. The Earth is becoming greatly over-crowded. This planet may be suitable for human   coloniziation."
T5$ = "Your Mission: Scout ahead and prepare the planet for inhabitation. If the planetis inhabited by other beings do not attempt to interfere.  Return to Earth and  report your findings."
T6$ = "You will be flying a highly advanced rocket. This rocket is equipped with the   most powerful warp engine available. It is also covered with a warp shield to   prevent the effects of high speed travel from damaging the rocket."
T7$ = "Good Luck!"

'Text being typed out
COLOR 2
PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T1$)
   PRINT MID$(T1$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO 500: NEXT Del
NEXT Z

COLOR 14
PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T2$)
   PRINT MID$(T2$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO 500: NEXT Del
NEXT Z

COLOR 15
PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T3$)
   PRINT MID$(T3$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO 500: NEXT Del
NEXT Z

FOR Del = 1 TO 50000: NEXT Del: CLS

COLOR 15
PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T4$)
   PRINT MID$(T4$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO RND * 500: NEXT Del
NEXT Z

COLOR 15
PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T5$)
   PRINT MID$(T5$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO RND * 500: NEXT Del
NEXT Z

COLOR 15
PRINT : PRINT : FOR Del = 1 TO 10000: NEXT Del
FOR Z = 1 TO LEN(T6$)
   PRINT MID$(T6$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO RND * 500: NEXT Del
NEXT Z

COLOR 15
PRINT : PRINT : FOR Del = 1 TO 100000: NEXT Del
COLOR 1
FOR Z = 1 TO LEN(T7$)
   PRINT MID$(T7$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO 500: NEXT Del
NEXT Z

PRINT : FOR Del = 1 TO 100000: NEXT Del

END SUB

SUB Launch

LINE (0, 130)-(180, 200), 0, BF
LOCATE 10, 2: COLOR 10
PRINT "Rocket is preparing"
LOCATE 11, 2
PRINT "to leave the"
LOCATE 12, 2
PRINT "atmosphere."

FOR X = 480 TO -480 STEP -1
   LET C = C + 1: IF C = 16 THEN LET C = 1
   LINE (320, X)-(270, X + 80), C          'nose up left
   LINE (370, X + 80)-(320, X), C          'nose right
   LINE (270, X + 80)-(370, X + 80), C    'nost bottom
   LINE (270, X + 190)-(180, X + 240), C    'upper left fin
   LINE (180, X + 240)-(270, X + 140), C
   LINE (370, X + 190)-(460, X + 240), C     'upper right fin
   LINE (460, X + 240)-(370, X + 140), C
   LINE (270, X + 80)-(270, X + 190), C       'left upper body line
   LINE (370, X + 80)-(370, X + 190), C       'right upper body line
   LINE (240, X + 430)-(270, X + 190), C      'left lower body fin
   LINE (400, X + 430)-(370, X + 190), C      'right lower body fin
   LINE (260, X + 390)-(240, X + 430), C      'bottom left interior
   LINE (380, X + 390)-(400, X + 430), C       'bottom right interior
   LINE (260, X + 390)-(380, X + 390), C       'bottom
   IF X > 150 THEN SOUND 32767, .03         'slows down rocket before fire
   SOUND (200 * RND) + (200 * RND) + 37, .03
   IF X < 150 THEN CALL Fire1(X)             'call fire routine when bottom is visible
   'erasing lines
   LINE (320, X)-(270, X + 80), C        'nose
   LINE (370, X + 80)-(320, X), C
   LINE (270, X + 190)-(180, X + 240), 0     'upper left fin
   LINE (269, X + 191)-(179, X + 241), 0    'upper left fin
   LINE (268, X + 192)-(178, X + 242), 0    'upper left fin
   LINE (370, X + 190)-(460, X + 240), 0       'upper right fin
   LINE (371, X + 191)-(461, X + 241), 0     'upper right fin
   LINE (372, X + 192)-(462, X + 242), 0     'upper right fin
   LINE (260, X + 390)-(240, X + 430), 0     'bottom
   LINE (380, X + 390)-(400, X + 430), 0
   LINE (260, X + 390)-(380, X + 390), 0
   CALL PsetStars1
NEXT X
END SUB

SUB LaunchBackGround

LINE (0, 300)-(640, 480), 6, BF       'ground
LINE (170, 300)-(470, 480), 2, BF      'grass

CIRCLE (600, 40), 100, 15              'moon
PAINT STEP(0, 0), 8, 15
CIRCLE (600, 40), RND * 20, 7       'craters on moon
CIRCLE (560, 10), RND * 20, 7
CIRCLE (530, 70), RND * 20, 7
CIRCLE (630, 90), RND * 20, 7
CIRCLE (580, 70), RND * 20, 7

END SUB

SUB MeltRay

LINE (470, 140)-(620, 192), 0, BF
LOCATE 10, 60: COLOR 10
PRINT "What kind of"
LOCATE 11, 60
PRINT "spacecraft is"
LOCATE 12, 60
PRINT "that?"

FOR Y = 640 TO -15 STEP -(420 / 93)
   CIRCLE (Y, 50), 30, 10
   PAINT (Y - 15, 50), 1, 10
   IF Y > 220 THEN       'Funeral March of a Marionette
      READ N, D
      SOUND N, D
   END IF
   IF Y > 450 AND Y < 455 THEN
      LINE (470, 140)-(620, 192), 0, BF
      LOCATE 10, 60: COLOR 4
      PRINT "Intruders! Use the"
      LOCATE 11, 60
      PRINT "atom destroying"
      LOCATE 12, 60
      PRINT "neutron ray!"
   END IF
   IF Y > 350 AND Y < 355 THEN
      LINE (470, 140)-(620, 192), 0, BF
      LOCATE 10, 60: COLOR 10
      PRINT "     NOOOO!!!!   "
      LOCATE 11, 60
      PRINT "We come in peace!"
      LOCATE 12, 60
      PRINT "  Don't kill us!"
   END IF
   IF Y > 218 AND Y < 223 THEN
      LINE (470, 140)-(620, 192), 0, BF
      LOCATE 11, 60: COLOR 4
      PRINT "  Charging Ray."
      FOR Ray = 79 TO 133 STEP .05
         C = C + 1: IF C = 16 THEN C = 1
         LINE (218, 79)-(222, Ray + 5), C, BF
         SOUND 100 * RND + 37, .04
      NEXT Ray
      FOR Cray = 1 TO 1200
         IF Cray = 10 THEN
            LINE (470, 140)-(620, 192), 0, BF
            LOCATE 11, 60: COLOR 4
            PRINT "Destroying Target."
         END IF
         C = C + 1: IF C = 16 THEN C = 1
         LINE (218, 79)-(222, Ray + 5), C, BF
         SOUND 100 * RND + 37, .04
      NEXT Cray
      LINE (218, 65)-(222, Ray + 5), 8, BF'erase ray
   END IF
   PAINT (Y, 50), 8, 10  'erasing
   CIRCLE (Y, 50), 30, 8
NEXT Y

END SUB

SUB PlanetBG
CLS

PAINT (1, 1), 8   'sky
LINE (0, 400)-(640, 480), 3, BF 'ground
PAINT STEP(0, 0), 3, 15
LINE (150, 350)-(290, 400), 6, BF   'landing box

CIRCLE (540, 300), 60, 14   ' roofs of buildings
PAINT STEP(0, 0), 14, 14
CIRCLE (440, 270), 60, 14
PAINT STEP(0, 0), 14, 14

LINE (480, 300)-(600, 410), 0, BF   'buildings
LINE (380, 270)-(500, 405), 0, BF
CIRCLE (560, 390), 20, 9, , , 1.3  'doors
CIRCLE (410, 385), 20, 9, , , 1.5
COLOR 15: LOCATE 18, 53            'building writings
PRINT "€ºêïô"
LOCATE 19, 58: PRINT "õ"
COLOR 15: LOCATE 20, 66
PRINT "œ­øè÷ðñ"

LINE (470, 140)-(620, 192), 0, BF
LOCATE 10, 60: COLOR 10
PRINT "Look! This planet"
LOCATE 11, 60
PRINT "appears to be"
LOCATE 12, 60
PRINT "inhabited."

END SUB

SUB PsetStars1

'keeps stars from being erased during launch
'(180-460, 0-300)
PSET (200, 150), 1
PSET (220, 20), 2
PSET (230, 110), 3
PSET (240, 250), 3
PSET (260, 295), 4
PSET (280, 50), 5
PSET (300, 100), 6
PSET (320, 200), 7
PSET (350, 80), 8
PSET (370, 280), 9
PSET (390, 150), 10
PSET (410, 40), 11
PSET (430, 290), 12
PSET (450, 100), 13
PSET (250, 150), 14
PSET (300, 259), 10
PSET (330, 20), 14

END SUB

SUB PsetStars2

'keeps stars from being erased during flying
'(0-640, 195-285)
PSET (320, 220), 1
PSET (50, 200), 2
PSET (100, 235), 3
PSET (150, 265), 4
PSET (200, 285), 5
PSET (250, 245), 6
PSET (300, 223), 7
PSET (350, 196), 8
PSET (400, 245), 9
PSET (450, 280), 10
PSET (500, 236), 11
PSET (550, 200), 12
PSET (600, 270), 13
PSET (639, 240), 15

END SUB

SUB RocketLand

FOR Del = 1 TO 50000: NEXT Del

FOR X = -250 TO 136 STEP 1
   IF X = -100 THEN
      LINE (470, 140)-(620, 192), 0, BF
      LOCATE 10, 60: COLOR 10
      PRINT "What could"
      LOCATE 11, 60
      PRINT "possibly survive"
      LOCATE 12, 60
      PRINT "here?"
   END IF
   LET C = C + 1: IF C = 16 THEN LET C = 1
   LINE (220, X)-(195, X + 40), C          'nose up left
   LINE (245, X + 40)-(220, X), C          'nose right
   LINE (195, X + 40)-(245, X + 40), C   'nose bottom
   LINE (195, X + 95)-(150, X + 120), C    'upper left fin
   LINE (150, X + 120)-(195, X + 70), C
   LINE (245, X + 95)-(290, X + 120), C     'upper right fin
   LINE (290, X + 120)-(245, X + 70), C
   LINE (195, X + 40)-(195, X + 95), C       'left upper body line
   LINE (245, X + 40)-(245, X + 95), C       'right upper body line
   LINE (180, X + 215)-(195, X + 95), C      'left lower body fin
   LINE (260, X + 215)-(245, X + 95), C      'right lower body fin
   LINE (190, X + 195)-(180, X + 215), C      'bottom left interior
   LINE (250, X + 195)-(260, X + 215), C       'bottom right interior
   LINE (190, X + 195)-(250, X + 195), C       'bottom
   FOR Del = 1 TO 200: NEXT Del
   'erasing lines
   LINE (150, X + 120)-(195, X + 70), 8   'upper left fin
   LINE (290, X + 120)-(245, X + 70), 8    'upper right fin
   LINE (180, X + 215)-(195, X + 95), 8      'left lower body fin
   LINE (260, X + 215)-(245, X + 95), 8      'right lower body fin
   LINE (220, X)-(195, X + 40), 8          'nose up left
   LINE (245, X + 40)-(220, X), 8          'nose right
NEXT X

LINE (470, 140)-(620, 192), 0, BF
LOCATE 10, 60: COLOR 10
PRINT "We had better"
LOCATE 11, 60
PRINT "hope they are"
LOCATE 12, 60
PRINT "peaceful beings."

FOR Del = 1 TO 100000: NEXT Del

END SUB

SUB RocketMelt

LINE (470, 140)-(620, 192), 0, BF
LOCATE 10, 60: COLOR 10
PRINT "     HELP!"
LOCATE 11, 60
PRINT "Our atoms are"
LOCATE 12, 60
PRINT "being destroyed."

DIM melt(3000)
FOR M = 9000 TO 1 STEP -1
   X = 140 + (RND * 140)
   Y = 136 + (RND * 190)
   GET (X, Y)-(X + 25, Y + 25), melt
   PUT (X, Y + 1), melt, PSET
   SOUND .5 * M * RND + 37, .03
   IF M = 7000 THEN LINE (470, 140)-(620, 192), 8, BF
   IF INKEY$ = " " THEN EXIT FOR
NEXT M

'erasing box - center on point (220,350)
FOR X1 = 0 TO 220 STEP .08
   X2 = X1 * (440 / 220)
   Y1 = X1 * (350 / 220)
   Y2 = X2 * (130 / 440)
   LINE (X1, Y1)-(640 - X2, 480 - Y2), 0, B
NEXT X1

END SUB

SUB Stars1

CLS
CALL PsetStars1
FOR Star = 1 TO 30
   LET C = C + 1: IF C = 16 THEN LET C = 1
   PSET (RND * 180, RND * 300), C          'left side stars
   PSET (640 - (RND * 180), RND * 300), C  'right side stars
NEXT Star

END SUB

SUB Stars2

CALL PsetStars2
FOR Star = 1 TO 40
   LET C = C + 1: IF C = 16 THEN LET C = 1
   PSET (RND * 640, RND * 195), C   'top stars
   PSET (RND * 640, 480 - (RND * 195)), C 'bottom stars
NEXT Star

END SUB

SUB Warp
SCREEN 9

War$ = "Prepare to enter warp speed travel!"
LOCATE 9, 40 - (.5 * LEN(War$)): PRINT War$
FOR Del = 1 TO 50000: NEXT Del

SCREEN 12
DO: LET W = W + 1
   FOR X = 1 TO 640 STEP 15
      LINE (640 - X, 480)-(X, 0), C
      LINE (640, 0)-(640 - X, 480), C
      LINE (X, 480)-(0, 0), C
   NEXT X
   IF C = 15 THEN
      LET C = 14
   ELSE
      C = 15
   END IF
LOOP UNTIL W = 30

'clear screen
FOR Z = 5 TO 1 STEP -1
   FOR X = 0 TO 320 STEP Z
      Y = 2 / 3 * X
      LINE (X, Y)-(640 - X, 480 - Y), 0, B
   NEXT X
NEXT Z

END SUB
