'erl   12.9.98      Christmas Tree
CLS
SCREEN 12       '640 x 480

COLOR 15                                ' **** puts snow in background
RANDOMIZE TIMER
FOR Snow = 1 TO 300
    DO
        LET Column = INT(RND * 100) + 1
        LET Row = INT(RND * 100) + 1
    LOOP UNTIL Row <= 28 AND Column <= 80
    LOCATE Row, Column
    PRINT "*"
NEXT Snow

LOCATE 1, 71
COLOR 12
PRINT "Eric Laslo"
COLOR 15

FOR TreeBody = 1 TO 18           ' **** loop that draws body as a series of
    IF TreeBody = 1 THEN         ' **** overlapping triangles that
        LET StartRow = 50        ' **** increase in size and move down
        LET LeftColumn = 300
        LET RightColumn = 340
        LET BottomLine = 60
    ELSE
        LET StartRow = StartRow + 10
        LET LeftColumn = LeftColumn - 8
        LET RightColumn = RightColumn + 8
        LET BottomLine = BottomLine + 20
    END IF
    LINE (320, StartRow)-(LeftColumn, BottomLine), 2
    LINE (LeftColumn, BottomLine)-(RightColumn, BottomLine), 2
    LINE (RightColumn, BottomLine)-(320, StartRow), 2
    PAINT (320, BottomLine - 1), 2
NEXT TreeBody

LINE (300, 400)-(340, 430), 6, BF     'trunk

LINE (0, 430)-(640, 480), 7, BF             'ground
LINE (0, 430)-(640, 430), , , &H1112        'snow on ground
LINE (0, 431)-(640, 431), , , &H1113
LINE (0, 432)-(640, 432), , , &H1114
LINE (0, 433)-(640, 433), , , &H1115
LINE (0, 434)-(640, 434), , , &H1116
LINE (0, 435)-(640, 435), , , &H1117
LINE (0, 436)-(640, 436), , , &H1118
LINE (0, 437)-(640, 437), , , &H1119
LINE (0, 438)-(640, 438), , , &H1110
LINE (0, 439)-(640, 439), , , &H111
LINE (0, 440)-(640, 440), , , &H112

'star
LINE (290, 40)-(350, 40), 14
LINE (290, 40)-(320, 60), 14
LINE (350, 40)-(320, 60), 14
LINE (320, 20)-(340, 70), 14
LINE (320, 20)-(300, 70), 14
LINE (300, 70)-(320, 60), 14
LINE (340, 70)-(320, 60), 14
PAINT (320, 50), 14
PAINT (320, 30), 14
PAINT (340, 41), 14
PAINT (300, 41), 14
PAINT (315, 60), 14
PAINT (325, 60), 14

'LET C = C                                    'sets color
CIRCLE (320, 75), 21, 4, 3.2, 6.2, 1 / 5      'decorations
CIRCLE (320, 90), 26, 9, 3.2, 6.2, 1 / 5
CIRCLE (320, 105), 30, 14, 3.2, 6.2, 1 / 5
CIRCLE (320, 120), 37, 4, 3.2, 6.2, 1 / 5
CIRCLE (320, 135), 47, 9, 3.2, 6.2, 1 / 5
CIRCLE (320, 150), 50, 14, 3.2, 6.2, 1 / 5
CIRCLE (320, 165), 55, 4, 3.2, 6.2, 1 / 5
CIRCLE (320, 180), 60, 9, 3.2, 6.2, 1 / 5
CIRCLE (320, 195), 70, 14, 3.2, 6.2, 1 / 5
CIRCLE (320, 210), 77, 4, 3.2, 6.2, 1 / 5
CIRCLE (320, 225), 82, 9, 3.2, 6.2, 1 / 5
CIRCLE (320, 240), 88, 14, 3.2, 6.2, 1 / 5
CIRCLE (320, 255), 96, 4, 3.2, 6.2, 1 / 5
CIRCLE (320, 270), 102, 9, 3.2, 6.2, 1 / 5
CIRCLE (320, 285), 106, 14, 3.2, 6.2, 1 / 5
CIRCLE (320, 300), 110, 4, 3.2, 6.2, 1 / 5
CIRCLE (320, 315), 120, 9, 3.2, 6.2, 1 / 5
CIRCLE (320, 330), 127, 14, 3.2, 6.2, 1 / 5
CIRCLE (320, 345), 130, 4, 3.2, 6.2, 1 / 5
CIRCLE (320, 360), 135, 9, 3.2, 6.2, 1 / 5
CIRCLE (320, 375), 144, 14, 3.2, 6.2, 1 / 6

DO UNTIL X = 9999 AND Y = 9999        'lights
    READ X, Y
    IF X = 9999 THEN EXIT DO
    DATA 300,80,290,200,240,400,300,130,310,300,340,210,360,250,370,140,360,315
    DATA 450,370,270,270,300,180,290,315,270,150,250,190,260,320,200,390
    DATA 400,270,320,390,290,380,345,380,330,90,350,130,360,180,370,350,340,370
    DATA 300,110,310,360,190,390
    DATA 9999,9999,9999
    DO                                'determines color
        LET C = INT(RND * 100)
    LOOP UNTIL C = 1 OR C = 4 OR C = 5 OR C = 10 OR C = 12 OR C = 14
    CIRCLE (X, Y), 3, C
    PAINT (X, Y), C
LOOP

DO WHILE song = 0
        dth1$ = "O3L4G.L8FL4EDCDEC"
        dth2$ = "O3L8DEFDL4E.L8DL4CO2BO3L2CL4G.L8FL4ED"
        dth3$ = "O3L4CDECL8DEFDL4E.L8DL4CO2BO3L2C"
        dth4$ = "O3L4D.L8EL4FDE.L8FL4GD"
        dth5$ = "O3L8EFL4GL8ABL4O4CO3BAL2GL4G.L8FL4ED"
        dth6$ = "O3L4CDECL8AAAAL4G.L8FL4EDL2C"
    PLAY dth1$ + dth2$ + dth3$ + dth4$ + dth5$ + dth6$
        oct1$ = "O3L4CL8F.L16FL4FGL8A.L16AL4A.L8AL8GAL4B-E"
        oct2$ = "O3L4GFCL8F.L16FL4FGL8A.L16AL4A.L8A"
        oct3$ = "O3L8GAL4B-EGFO4L8P8CL8CO3AO4L4D.L8C"
        oct4$ = "O4L8CO3B-L4B-.L8B-B-GO4L4C.O3L8B-B-AL4A"
        oct5$ = "O3L4CL8F.L16FL4FGL8A.L16AL4A.L8AL8GAL4B-EGF"
    PLAY oct1$ + oct2$ + oct3$ + oct4$ + oct5$
LOOP

COLOR 15
END
