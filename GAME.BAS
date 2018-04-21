'Erl
'4-28-99, 5-28-99
'Game
DECLARE SUB Intro ()
DECLARE SUB HighScores ()
DECLARE SUB LevelSet ()
DECLARE SUB Report ()
DECLARE SUB Ship ()
DECLARE SUB AtomBomb ()
DECLARE SUB PlaceEnemies ()
DECLARE SUB Shoot ()
SCREEN 12   '640x480 pixel 28x77 text
RANDOMIZE TIMER
' **** data for 4 different ship designs
DATA " ³ ", "èèè", "¶ºÇ"
DATA "*^*", "°²°", ""
DATA " ³ ", "ÝèÞ", "ßºß"
DATA "©³ª", "Éè»", "ÈÍ¼"
DIM SHARED X, Y                 'text location axis for sprites
DIM SHARED ShipSprite$(1 TO 12)        '4 different ships at 3 lines each
DIM SHARED EnemySpot$(1 TO 26, 2 TO 80) 'enemies locations
DIM SHARED Shots, Hits, Score, Bombs, Bhits, ShipType, Level, Enemies
DIM SHARED Scores(1 TO 100), Dat$(1 TO 100), Name$(1 TO 100)
KEY 15, CHR$(0) + CHR$(57)     'creates space bar key code
KEY 16, CHR$(0) + CHR$(30)    'creates A (bomb) key code

FOR Ships = 1 TO 12          '**** reads data for ship
   READ ShipSprite$(Ships)
NEXT Ships

CALL Intro
CALL LevelSet

Game:
KEY(12) ON                    '**** enables trapping for left key
KEY(13) ON                    '**** enables trapping for right key
KEY(15) ON                    '**** enables trapping for space bar
KEY(16) ON                    '**** enables trapping for atom bomb key
DO UNTIL INKEY$ = CHR$(27) OR (Shots > 100) OR (Enemies = Hits + Bhits)
   ON KEY(12) GOSUB ShipLeft
   ON KEY(13) GOSUB ShipRight
   ON KEY(15) GOSUB ShootGun
   ON KEY(16) GOSUB ABomb
LOOP
KEY(12) OFF
KEY(13) OFF
KEY(15) OFF
KEY(16) OFF

COLOR 15
LOCATE 24, 1: PRINT "Press Any Key to Continue"
WHILE INKEY$ = "": WEND
CALL Report

IF Level < 4 THEN      ' **** creates four levels
   CALL LevelSet
   GOTO Game
END IF

CALL HighScores

COLOR 7
END

ShipLeft:
   LINE (0, 400)-(640, 480), 0, BF         'erase ship
   IF X > 2 THEN X = X - 1
   CALL Ship
   RETURN
ShipRight:
   LINE (0, 400)-(640, 480), 0, BF         'erase ship
   IF X < 77 THEN X = X + 1
   CALL Ship
   RETURN
ShootGun:
   CALL Shoot
   RETURN
ABomb:
   IF (Bombs > 0) AND (X < 77) THEN CALL AtomBomb
   RETURN

'******************************Sub to display atom bomb***********************
SUB AtomBomb

KEY(12) STOP
KEY(13) STOP
KEY(15) STOP
FOR Bomb = (Y - 2) TO 1 STEP -1
   COLOR 15
   LOCATE Bomb, X
   PRINT "o"
   FOR Del = 1 TO 1500: NEXT Del
   LOCATE Bomb, X: PRINT " "
   IF EnemySpot$(Bomb, X) = "é" THEN
      I = X: IF I < 6 THEN I = 6: IF I > 72 THEN I = 72
      J = Bomb: IF J < 5 THEN J = 5
      FOR X2 = (I - 4) TO (I + 4)
         FOR Z = (J - 3) TO (J + 3)
            FOR Explode = 1 TO 4
               SOUND 100, .03
            NEXT Explode
            IF EnemySpot$(Z, X2) = "é" THEN
               Bhits = Bhits + 1
               EnemySpot$(Z, X2) = ""
               IF RND > .5 THEN COLOR 8:  ELSE COLOR 7
               LOCATE Z, X2
               PRINT "ú"
            END IF
         NEXT Z
      NEXT X2
      EXIT FOR
   END IF
NEXT Bomb
KEY(12) ON
KEY(13) ON
KEY(15) ON

Bombs = Bombs - 1 ' **** sets only 15 bombs
Shots = Shots + 1

END SUB

'***************************Sub to keep a record of high scores***************
SUB HighScores

CLS
COLOR 12
PRINT "Enter your name for the records:   ";
COLOR 3
INPUT NameInput$
CLS

OPEN "A:\GameHigh.dat" FOR APPEND AS #1
WRITE #1, Score, DATE$, NameInput$
CLOSE #1

OPEN "A:\GameHigh.dat" FOR INPUT AS #1
DO
   k = k + 1
   INPUT #1, Scores(k), Dat$(k), Name$(k)
LOOP UNTIL EOF(1)
CLOSE #1

FOR I = 1 TO (k - 1)
   FOR J = (I + 1) TO k
      IF Scores(I) < Scores(J) THEN
         SWAP Scores(J), Scores(I)
         SWAP Dat$(J), Dat$(I)
         SWAP Name$(J), Name$(I)
      END IF
   NEXT J
NEXT I

COLOR 14
PRINT "Name"; TAB(20); "Date"; TAB(40); "Score"
COLOR 1
PRINT "============================================="
IF k > 21 THEN k = 21
FOR Z = 1 TO k
   IF (Name$(Z) = NameInput$) AND (Scores(Z) = Score) AND (Dat$(Z) = DATE$) THEN
      COLOR 15
      PRINT Name$(Z); TAB(20); Dat$(Z); TAB(40); USING "####"; Scores(Z)
   ELSE
      COLOR 12
      PRINT Name$(Z);
      COLOR 10
      PRINT TAB(20); Dat$(Z);
      COLOR 7
      PRINT USING "####"; TAB(40); Scores(Z)
   END IF
NEXT Z

END SUB

'****************************Display Intro Screen****************************
SUB Intro

'Data for text
T1$ = "Eric Laslo"
T2$ = "Game -> Space Exterminator"
T3$ = "5.28.99"
T4$ = " Space Exterminator "
T5$ = "     You are part of a colonization mission to another planet. Your objective isto clear the path for the main spaceship. You are the pilot of a small fighter  ship that is capable of firing two types of guns. Your primary gun is a laser   that is able to destroy up two to targets in a row. You are also equipped with  high powered atom bombs. These will destroy an area of 8x6 squares. Use them    wisely, supplies are limited. After each level you receive a new ship. Be       careful, these aliens melted the last ship we sent in. If you finish            successfully you will win a coveted place in the hall of fame."

' **** Text being typed out
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

FOR Del = 1 TO 77000: NEXT Del: CLS

FOR Bugs = 1 TO 80    ' **** puts bugs on screen for bouncing words
   IF RND > .6 THEN
      LOCATE 14, Bugs
      COLOR RND * 14 + 1
      PRINT "é"
   END IF
NEXT Bugs
Row = 40 - (.5 * LEN(T4$))
FOR Down = 1 TO 14         ' **** words go down the screen
   COLOR 15
   LOCATE Down, Row
   PRINT T4$
   FOR Del = 1 TO 6000: NEXT Del
   IF Down <> 14 THEN COLOR 0
   LOCATE Down, Row
   PRINT T4$
NEXT Down
FOR Del = 1 TO 2000: NEXT Del
FOR Across = 1 TO 420       ' **** words bounce across the screen
   IF C = 15 THEN C = 1:  ELSE C = C + 1
   IF C = 8 THEN C = C + 1
   COLOR C
   IF RowFlag = 1 THEN
      Row = Row - 1
      IF Row = 1 THEN RowFlag = 0
   ELSEIF RowFlag = 0 THEN
      Row = Row + 1
      IF Row = 61 THEN RowFlag = 1
   END IF
   LOCATE 14, Row
   PRINT T4$
   SOUND RND * 1000 + 37, .3
NEXT Across
FOR Up = 14 TO 1 STEP -1         ' **** words go up the screen
   COLOR 15
   LOCATE Up, Row
   PRINT T4$
   FOR Del = 1 TO 6000: NEXT Del
   COLOR 0
   LOCATE Up, Row
   PRINT T4$
NEXT Up

LOCATE 1, 33
COLOR 10
PRINT "Space Exterminator"
PRINT : PRINT : PRINT

COLOR 15
FOR Z = 1 TO LEN(T5$)
   PRINT MID$(T5$, Z, 1);
   SOUND 100, .1
   FOR Del = 1 TO RND * 500: NEXT Del
NEXT Z

PRINT : PRINT
COLOR 1
PRINT "Good Luck!!!"
FOR Del = 1 TO 50000: NEXT Del
PRINT : PRINT : PRINT : PRINT
COLOR 14
PRINT "Key Assignments (num lock MUST be off) :"
PRINT
COLOR 9
PRINT "Move Ship Left or Right:   Arrows on the number pad"
PRINT "Shoot:   Space Bar"
PRINT "Launch Atom Bomb:   <a> key"
PRINT "Advance to Next Level:   <ESC> key"
PRINT : PRINT : PRINT

COLOR 4
PRINT "Press Any Key To Begin"
WHILE INKEY$ = "": WEND

END SUB

'*************************Sub to setup each level*****************************
SUB LevelSet

CLS
Level = Level + 1
COLOR 14
LOCATE 12, 38: PRINT "Level"; Level
FOR Del = 1 TO 130000
   IF INKEY$ <> "" THEN EXIT FOR
NEXT Del
CLS

X = 40              '**** resets all the data types
Y = 27
Bombs = 15
Enemies = 0
Shots = 0
Hits = 0
Bhits = 0

IF Level = 1 THEN ShipType = 1   '**** selects ship type for each level
IF Level = 2 THEN ShipType = 4
IF Level = 3 THEN ShipType = 7
IF Level = 4 THEN ShipType = 10

CALL PlaceEnemies
CALL Ship

END SUB

'******************************Sub to setup enemies***************************
SUB PlaceEnemies

FOR Col = 1 TO 20        'clears column
   FOR Row = 2 TO 77     'clears row
      EnemySpot$(Col, Row) = ""
   NEXT Row
NEXT Col

Enemies = 0
FOR Col = 1 TO 20        'places by column
   FOR Row = 2 TO 77     'places by row
      COLOR INT(15 * RND)
      IF RND > (1 - (Level * .05)) THEN
         EnemySpot$(Col, Row) = "é"
         Enemies = Enemies + 1
      END IF
      LOCATE Col, Row
      PRINT EnemySpot$(Col, Row)
   NEXT Row
NEXT Col

END SUB

'*********************************Sub to Display report**********************
SUB Report

' **** erasing box - center on point (320,240)
FOR X = 0 TO 320 STEP .15
   Y = X * (240 / 320)
   LINE (X, Y)-(640 - X, 480 - Y), 0, B
NEXT X

TotalShots = TotalShots + Shots
TotalHits = Hits + Bhits
Score = Score + TotalHits
IF Shots < 1 THEN Shots = 1

CLS
COLOR 2
PRINT "Number of Shots Made";
COLOR 15
PRINT Shots
COLOR 2
PRINT "Total Hits";
COLOR 15
PRINT TotalHits
COLOR 2
PRINT "Hit Percentage";
COLOR 15
PRINT (TotalHits / Shots) * 100
PRINT : PRINT
COLOR 2
PRINT "Total Score"
COLOR 15
PRINT Score

COLOR 15
LOCATE 24, 1: PRINT "Press Any Key to Continue"
WHILE INKEY$ = "": WEND

END SUB

'********************************Sub to place and move ship*******************
SUB Ship

KEY(12) OFF
KEY(13) OFF
KEY(15) OFF
KEY(16) OFF

COLOR 10
LOCATE Y - 1, X - 1
PRINT ShipSprite$(ShipType)
LOCATE Y, X - 1
PRINT ShipSprite$(ShipType + 1)
LOCATE Y + 1, X - 1
PRINT ShipSprite$(ShipType + 2)

KEY(12) ON
KEY(13) ON
KEY(15) ON
KEY(16) ON

END SUB

'*********************************Sub to shoot gun***************************
SUB Shoot

KEY(12) STOP
KEY(13) STOP
KEY(16) STOP
Z = 0
Shots = Shots + 1
FOR Bullet = (Y - 2) TO 1 STEP -1
   COLOR 12
   LOCATE Bullet, X
   PRINT "ï"
   FOR Del = 1 TO 300: NEXT Del
   LOCATE Bullet, X: PRINT " "
   IF EnemySpot$(Bullet, X) = "é" THEN
      Z = Z + 1
      Hits = Hits + 1
      SOUND 3000, .3
      IF RND > .5 THEN COLOR 8:  ELSE COLOR 7
      LOCATE Bullet, X
      PRINT "ú"
      EnemySpot$(Bullet, X) = ""
      IF Z = 2 THEN EXIT FOR
   END IF
NEXT Bullet
KEY(12) ON
KEY(13) ON
KEY(16) ON

END SUB
