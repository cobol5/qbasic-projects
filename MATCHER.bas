'Mathletes Be My Valentine Matchup Version 3.0
'QBasic programming by erl, adapted from old GWBasic code
'Original GWBasic program by SW and others?
'1999-2000
'2.3.2000 - 2.11.2000 ,2.5.2001
'Erl
'                                       
'To Do? :
'Error control for file open
'A blank entry of XXX won't sort and won't work right in analyze
'Must be a minor bug? Checked original code, can't find anything,
'The more stuff I add the more problems come up
'
'To enter the data will be time consuming. Figure 40s per data sheet.
'To do a full run of three grades takes about 3 hours. You should save to a file incase
'there is a problem but now it only saves all at the end. To expedite the
'date entering process you can split up as much as you want. Just have a
'bunch of files and cut and paste them all together to a new one with notepad,
'and add the corresponding numbers at the beginning. Do each grade at a
'separate computer and you can add them up however you want.

DECLARE SUB Menu ()
DECLARE SUB Sort ()
DECLARE SUB ReadFromFile ()
DECLARE SUB LinePrint ()
DECLARE SUB PrintOnScreen ()
DECLARE SUB Analyze ()
DECLARE SUB DataEnter ()
DECLARE SUB Intro ()
DECLARE SUB WriteOutputFile ()
DIM SHARED DT$
DIM SHARED RunOption, PrintOption
DIM SHARED B, G
DIM SHARED P, W, HEAD$
DIM SHARED a
DIM SHARED H$
'**** other dimensions after main option selection
H$ = CHR$(3)   'Heart
DT$ = DATE$ + TIME$
CLS

CALL Intro

CLS
PRINT
PRINT "Select Option :"
PRINT
PRINT "     1=Enter Data"
PRINT "     2=Enter Data and Save it to a File"
PRINT "     3=Read Data From A File And Analyze it"
'PRINT "     4=Append More Data To a File"
DO
   RunOption = VAL(INKEY$)
LOOP UNTIL RunOption >= 1 AND RunOption <= 3
CLS

SELECT CASE RunOption
   CASE 1
      INPUT "Enter the Number of Boys "; B
      INPUT "Enter the Number of Girls "; G

      DIM SHARED U(B + G + 10) '**** arbitrary dimension
      DIM SHARED B$(B + 10), G$(G + 10)
      IF G > B THEN DIM SHARED a(G + 10) ELSE DIM SHARED a(B + 10)
      DIM SHARED B(B + G + 10) '**** arbitrary dimension
      CALL DataEnter
   CASE 2
      PRINT "Enter EXACT name to save as, put A:\ first"
      INPUT FileOut$
      FileOut$ = LTRIM$(RTRIM$(FileOut$))
      OPEN FileOut$ FOR OUTPUT AS #1
      PRINT : PRINT
      INPUT "Enter the Number of Boys "; B
      INPUT "Enter the Number of Girls "; G
      WRITE #1, B, G, DT$

      DIM SHARED U(B + G + 10) '**** arbitrary dimension
      DIM SHARED B$(B + 10), G$(G + 10)
      IF G > B THEN DIM SHARED a(G + 10) ELSE DIM SHARED a(B + 10)
      DIM SHARED B(B + G + 10)'**** arbitrary dimension
     
      CALL DataEnter
   CASE 3
      PRINT "Enter EXACT name of file to open, put A:\ first"
      INPUT FileOpen$
      FileOpen$ = LTRIM$(RTRIM$(FileOpen$))
      OPEN FileOpen$ FOR INPUT AS #1
      INPUT #1, B, G, DT$
      DIM SHARED U(B + G + 10) '**** arbitrary dimension
      DIM SHARED B$(B + 10), G$(G + 10)
      IF G > B THEN DIM SHARED a(G + 10) ELSE DIM SHARED a(B + 10)
      CALL ReadFromFile
      DIM SHARED B(B + G + 10) '**** arbitrary dimension
   'CASE 4
END SELECT

CALL Menu
CALL Analyze

SUB Analyze

P = B
Q = G
W = 1

StartAnalyze:  '**** begin to analyze here

FOR X = 1 TO P
   'IF W = 1 AND B$(X) = "XXX" THEN GOTO NextPerson
   'IF W = 0 AND G$(X) = "XXX" THEN GOTO NextPerson
   FOR Y = 1 TO Q
      a(Y) = 0
      FOR C = 1 TO 30   '**** compare all data for 2 people
         'IF W = 1 AND G$(Y) = "XXX" THEN A(Y) = 666  '**** sets for ignore
         'IF W = 0 AND B$(Y) = "XXX" THEN A(Y) = 666  '*** X data strings
         SELECT CASE W
            CASE 0      '**** for girls
               IF MID$(G$(X), C, 1) = MID$(B$(Y), C, 1) THEN a(Y) = a(Y) + 1
            CASE 1     '**** for boys
               IF MID$(B$(X), C, 1) = MID$(G$(Y), C, 1) THEN a(Y) = a(Y) + 1
         END SELECT
      NEXT C
   NEXT Y
   FOR V = 1 TO 10       '**** arrange top matches for people
      IF V > Q THEN GOTO PrePrint
      B(V) = 0
      FOR I = 1 TO Q
         IF a(I) = 666 THEN GOTO NextSort
         IF a(I) >= B(V) THEN LET B(V) = a(I): U(V) = I
NextSort:
      NEXT I
      a(U(V)) = -1
   NEXT V
PrePrint:          '**** set heading
   SELECT CASE W
      CASE 1   '**** for boys
         HEAD$ = (RIGHT$(B$(X), LEN(B$(X)) - 35)) + "    ROOM " + MID$(B$(X), 31, 3)
      CASE 0   '**** for girls
         HEAD$ = (RIGHT$(G$(X), LEN(G$(X)) - 35)) + "    ROOM " + MID$(G$(X), 31, 3)
   END SELECT
PrintResults:
   SELECT CASE PrintOption
   CASE 1
      CALL PrintOnScreen
   CASE 2
      CALL PrintOnScreen
      CALL LinePrint
   CASE 3
      CALL LinePrint
   CASE 4
      CALL WriteOutputFile
   END SELECT

NextPerson:
NEXT X

IF W = 1 THEN     '**** start analyzing for girls
   W = 0
   P = G
   Q = B
   GOTO StartAnalyze
END IF

IF PrintOption = 4 THEN CLOSE #3
CALL Menu

END SUB

SUB DataEnter

W = 1: P = B: GB$ = "BOY #"  '**** start by entering data for boys

EnterData:      '**** enter data

FOR X = 1 TO P
StudentEnter:   '**** return here if data is wrong
   CLS
   COLOR 4
   FOR Z = 1 TO 40       '**** header of hearts
      PRINT H$; " ";
   NEXT Z
   COLOR 7
   PRINT : PRINT
   PRINT "Enter name of ";
   COLOR 10
   PRINT GB$; X
   COLOR 15
   INPUT N$
   N$ = RTRIM$(LTRIM$(N$))
   IF N$ = "" THEN N$ = GB$ + " " + STR$(X)

   IF UCASE$(N$) = "XXX" THEN
      IF W = 1 THEN        '**** create empty data string, boys
         B$(X) = "XXX"
         IF RunOption = 2 THEN WRITE #1, B$(X)
      ELSE       '**** girls
         G$(X) = "XXX"
         IF RunOption = 2 THEN WRITE #1, G$(X)
      END IF
      GOTO NextData
   END IF

   COLOR 7
   PRINT
   DO                  '**** enter HR #
      PRINT "Enter Homeroom Number :"
      COLOR 15
      INPUT HR$
      COLOR 7
      IF LEN(HR$) <> 3 THEN PRINT : PRINT : PRINT "Must Have 3 Digits!": PRINT
   LOOP UNTIL LEN(HR$) = 3
   PRINT
   DO                  '**** enter grade
      PRINT "Enter Grade :"
      COLOR 15
      INPUT GRA$
      COLOR 7
      IF LEN(GRA$) <> 2 THEN PRINT : PRINT : PRINT "Must have 2 Digits!": PRINT
   LOOP UNTIL LEN(GRA$) = 2
   PRINT
   DO               '**** enter data string
      PRINT "Enter Data For ";
      COLOR 15
      PRINT N$
      PRINT
      PRINT "  ";
      COLOR 12
      FOR Z = 1 TO 30
         PRINT CHR$(25);
      NEXT Z
      PRINT
      COLOR 15
      INPUT D$
      IF LEN(D$) <> 30 THEN PRINT : PRINT : PRINT "Must have 30 Digits!": PRINT
   LOOP UNTIL LEN(D$) = 30

   CLS             '**** verify data is correct
   COLOR 15
   PRINT N$; "    "; HR$; "    "; GRA$; "    "; D$
   COLOR 7
   PRINT
   PRINT "Is this Data Correct (1=No ENTER=Yes)"
   DO
      IN$ = INKEY$
   LOOP UNTIL IN$ = "1" OR IN$ = CHR$(13)
   IF IN$ = "1" THEN GOTO StudentEnter

   IF W = 1 THEN        '**** create data string, boys
      B$(X) = D$ + HR$ + GRA$ + N$
      IF RunOption = 2 THEN WRITE #1, B$(X)
   ELSE       '**** girls
      G$(X) = D$ + HR$ + GRA$ + N$
      IF RunOption = 2 THEN WRITE #1, G$(X)
   END IF

NextData:
NEXT X

IF W = 1 THEN        '**** switch to entering data for girls
   W = 0
   P = G
   GB$ = "GIRL #"
   GOTO EnterData:
END IF

CLOSE

CALL Sort

END SUB

SUB Intro

COLOR 4
FOR X = 1 TO 40
   PRINT H$; " ";
NEXT X
PRINT : PRINT

COLOR 12
PRINT "        Welcome to Mu Alpha Theta's Be My Valentine Matchup Program!"
PRINT
COLOR 15
PRINT "        This Program will match up and print the compatibility of"
PRINT "           of the entered boys and girls based on their answers"
PRINT "                             to a questionnaire."
PRINT
PRINT "To begin you must have an EXACT count of boys and girls to be entered."
PRINT "There must be 3 digits for homeroom, 2 digits for grade, and"
PRINT "30 digits for data."
PRINT "I would recommend you add about 10 to your count of boys and girls."
PRINT "Do not put commas or other separators in the data."
PRINT
PRINT "The program will sort alphabetically by first name. All boys will print"
PRINT "first then all girls. All data must be entered before printing begins."
PRINT
COLOR 7
PRINT "Press Any Key to go on"
WHILE INKEY$ = "": WEND
CLS
                               
END SUB

SUB LinePrint

X$ = H$ + " COMPUTERIZED MATCH MADE FOR " + H$
LPRINT TAB(40 - (.5 * LEN(X$))); X$
LPRINT : LPRINT : LPRINT
LPRINT TAB(40 - (.5 * LEN(HEAD$))); HEAD$
PRINT HEAD$
LPRINT : LPRINT : LPRINT
LPRINT "      MU ALPHA THETA'S *Be My Valentine* computerized match program"
LPRINT
LPRINT " has analyzed your interest based on your responses to our questionaire"
LPRINT
LPRINT " Listed below you will find the names of 10 people with whom you are most"
LPRINT
LPRINT " compatible. The compatibility index following the name of each person"
LPRINT
LPRINT " on your list shows the degree of compatibility on a scale of 1 to 100."
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT "  Name"; TAB(34); "Grade   HR#        Compatibility"
LPRINT "------------------------------------------------------------------"
lps = 23

SELECT CASE W
   CASE 1   '**** print for boy
      FOR V = 1 TO 10
         IF V > G THEN GOTO PrintEndLine
         IF G$(U(V)) = "XXX" THEN GOTO PrintEndLine
         LET N = LEN(G$(U(V))) - 35
         LET N$ = RIGHT$(G$(U(V)), N)
         LET GRA$ = MID$(G$(U(V)), 34, 2)
         LET HR$ = MID$(G$(U(V)), 31, 3)
         LET I = (INT(B(V) / 30 * 1000 + .5)) / 10
         LPRINT
         LPRINT N$; TAB(35); GRA$; "    "; HR$; "            "; USING "###.#%"; I
      NEXT V
   CASE 0   '**** print for girl
      FOR V = 1 TO 10
         IF V > B THEN GOTO PrintEndLine
         IF B$(U(V)) = "XXX" THEN GOTO PrintEndLine
         LET N = LEN(B$(U(V))) - 35
         LET N$ = RIGHT$(B$(U(V)), N)
         LET GRA$ = MID$(B$(U(V)), 34, 2)
         LET HR$ = MID$(B$(U(V)), 31, 3)
         LET I = (INT(B(V) / 30 * 1000 + .5)) / 10
         LPRINT
         LPRINT N$; TAB(35); GRA$; "    "; HR$; "            "; USING "###.#%"; I
      NEXT V
END SELECT

PrintEndLine:
FOR Z = 1 TO (11 - V)  '**** fill in if not enough people
   LPRINT : LPRINT H$ + " No Match Available"
NEXT Z

lps = lps + 20
LPRINT : LPRINT : LPRINT : LPRINT : LPRINT : LPRINT : LPRINT : LPRINT
X1$ = "Thank You for participating in"
X2$ = "MU ALPHA THETA'S"
X3$ = H$ + " Be My Valentine Matchup " + H$
LPRINT TAB(40 - (.5 * LEN(X1$))); X1$
LPRINT TAB(40 - (.5 * LEN(X2$))); X2$
LPRINT TAB(40 - (.5 * LEN(X3$))); X3$

lps = lps + 11            '**** fill in the rest of the page
FOR FillPage = lps TO 59
   LPRINT
NEXT FillPage

END SUB

SUB Menu

CLS
PRINT
PRINT "Select Option :"
PRINT
PRINT "     1=Print All On Screen"
PRINT "     2=Print All On Screen And Printer"
PRINT "     3=Print All To Printer"
PRINT "     4=Write output to file"
PRINT
PRINT "     5=Exit Program"
DO
   IN = VAL(INKEY$)
LOOP UNTIL IN >= 1 AND IN <= 5
IF IN = 5 THEN
   PRINT : PRINT : PRINT : PRINT
   COLOR 4
   FOR X = 1 TO 40
      PRINT H$; " ";
   NEXT X
   PRINT : PRINT
   COLOR 15
   PRINT ""
   PRINT ""
   END
END IF
CLS
IF IN = 1 THEN
   PrintOption = 1
   CALL Analyze
ELSEIF IN = 2 THEN
   PrintOption = 2
   CALL Analyze
ELSEIF IN = 3 THEN
   PrintOption = 3
   CALL Analyze
ELSEIF IN = 4 THEN
   PrintOption = 4
   OPEN "output.dat" FOR OUTPUT AS #3
   CALL Analyze
END IF
PRINT : PRINT
'IF IN = 4 THEN PRINT "Search Unavailiable": PRINT : PRINT : CALL Menu

END SUB

SUB PrintOnScreen

CLS
X$ = H$ + " COMPUTERIZED MATCH MADE FOR " + H$
PRINT TAB(40 - (.5 * LEN(X$))); X$
COLOR 15
PRINT TAB(40 - (.5 * LEN(HEAD$))); HEAD$
COLOR 7
PRINT : PRINT
PRINT "  Name"; TAB(25); "         Grade   HR#        Compatibility"
PRINT
COLOR 15
SELECT CASE W
   CASE 1   '**** print for boy
      FOR V = 1 TO 10
         IF V > G THEN GOTO PrintEnd
         IF G$(U(V)) = "XXX" THEN GOTO PrintEnd
         LET N = LEN(G$(U(V))) - 35
         LET N$ = RIGHT$(G$(U(V)), N)
         LET GRA$ = MID$(G$(U(V)), 34, 2)
         LET HR$ = MID$(G$(U(V)), 31, 3)
         LET I = (INT(B(V) / 30 * 1000 + .5)) / 10
         PRINT H$; " "; N$; TAB(35); GRA$; "    "; HR$; "            "; USING "###.#%"; I
      NEXT V
   CASE 0   '**** print for girl
      FOR V = 1 TO 10
         IF V > B THEN GOTO PrintEnd
         IF B$(U(V)) = "XXX" THEN GOTO PrintEnd
         LET N = LEN(B$(U(V))) - 35
         LET N$ = RIGHT$(B$(U(V)), N)
         LET GRA$ = MID$(B$(U(V)), 34, 2)
         LET HR$ = MID$(B$(U(V)), 31, 3)
         LET I = (INT(B(V) / 30 * 1000 + .5)) / 10
         PRINT H$; " "; N$; TAB(35); GRA$; "    "; HR$; "            "; USING "###.#%"; I
      NEXT V
END SELECT

PrintEnd:
FOR Z = 1 TO (11 - V)
   PRINT H$ + "   No Match Available"
NEXT Z
PRINT : PRINT

COLOR 12
X1$ = "Thank You for participating in"
X2$ = "MU ALPHA THETA'S"
X3$ = H$ + " Be My Valentine Matchup " + H$
PRINT TAB(40 - (.5 * LEN(X1$))); X1$
PRINT TAB(40 - (.5 * LEN(X2$))); X2$
PRINT TAB(40 - (.5 * LEN(X3$))); X3$
COLOR 7

PRINT

IF PrintOption = 1 THEN PRINT "Press space to continue, P to print, ESC to exit"
IF PrintOption = 2 THEN PRINT "Press space to print & continue, ESC to exit"
DO
   IN$ = INKEY$
LOOP UNTIL IN$ = CHR$(27) OR IN$ = " " OR (UCASE$(IN$) = "P" AND PrintOption = 1)

IF IN$ = CHR$(27) THEN
   CALL Menu
END IF
IF UCASE$(IN$) = "P" THEN
   CALL LinePrint
   PRINT "Report Printed, pick next option"
END IF

END SUB

SUB ReadFromFile
    
DO
   FOR X = 1 TO B
      INPUT #1, B$(X)
   NEXT X
   FOR X = 1 TO G
      INPUT #1, G$(X)
   NEXT X
   IF NOT EOF(1) THEN
      PRINT "Not EOF!"
      PRINT "There was an error processing the file."
      PRINT "There appears to be more data in the file than was"
      PRINT "set at the beginning of the file. Or the file does not exist"
      PRINT "Continue as normal or make sure the data is what you want."
      PRINT : PRINT "Continue? Press any key"
      WHILE INKEY$ = "": WEND
   END IF
LOOP UNTIL EOF(1): CLOSE #1

PRINT
FOR X = 1 TO B             '**** display inputed data
   PRINT B$(X)
NEXT X
PRINT
FOR X = 1 TO G
   PRINT G$(X)
NEXT X
PRINT : PRINT
PRINT "Data was entered, Press any key to continue"
WHILE INKEY$ = "": WEND

CALL Sort

END SUB

SUB Sort

W = 1
P = B

SortChange:

FOR I = 1 TO (P - 1)      'sort
   FOR j = I + 1 TO P
      IF W = 1 THEN
         'C1$ = (MID$(B$(i), 31, 3)) '**** sort by HR
         'C2$ = (MID$(B$(j), 31, 3))
         IF B$(I) = "XXX" OR B$(j) = "XXX" THEN GOTO SkipSwap
         C1$ = (RIGHT$(B$(I), LEN(B$(I)) - 35)) '**** sort by first name
         C2$ = (RIGHT$(B$(j), LEN(B$(j)) - 35))
      ELSEIF W = 0 THEN
         'C1$ = (MID$(G$(i), 31, 3))        '**** sort by HR
         'C2$ = (MID$(G$(j), 31, 3))
         IF G$(I) = "XXX" OR G$(j) = "XXX" THEN GOTO SkipSwap
         C1$ = (RIGHT$(G$(I), LEN(G$(I)) - 35)) '**** sort by first name
         C2$ = (RIGHT$(G$(j), LEN(G$(j)) - 35))
      END IF
      IF W = 1 AND C1$ > C2$ THEN
         SWAP B$(j), B$(I)
      ELSEIF W = 0 AND C1$ > C2$ THEN
         SWAP G$(j), G$(I)
      END IF
SkipSwap:
   NEXT j
NEXT I

CLS                        '**** display sorted data
COLOR 7
FOR k = 1 TO P
   IF W = 1 THEN PRINT B$(k)
   IF W = 0 THEN PRINT G$(k)
NEXT k

PRINT
PRINT "Data was sorted, Press any key to continue"
WHILE INKEY$ = "": WEND

IF W = 1 THEN
   W = 0
   P = G
   GOTO SortChange
END IF

END SUB

SUB WriteOutputFile

PRINT HEAD$
PRINT #3, CHR$(13)
PRINT #3, CHR$(13)
PRINT #3, X$
PRINT #3, HEAD$
SELECT CASE W
   CASE 1   '**** print for boy
      FOR V = 1 TO 10
         IF V > G THEN GOTO PrintEndOutput
         IF G$(U(V)) = "XXX" THEN GOTO PrintEndOutput
         LET N = LEN(G$(U(V))) - 35
         LET N$ = RIGHT$(G$(U(V)), N)
         LET GRA$ = MID$(G$(U(V)), 34, 2)
         LET HR$ = MID$(G$(U(V)), 31, 3)
         LET I = (INT(B(V) / 30 * 1000 + .5)) / 10
         PRINT #3, H$; " "; N$; TAB(35); GRA$; "    "; HR$; "            "; USING "###.#%"; I
      NEXT V
   CASE 0   '**** print for girl
      FOR V = 1 TO 10
         IF V > B THEN GOTO PrintEndOutput
         IF B$(U(V)) = "XXX" THEN GOTO PrintEndOutput
         LET N = LEN(B$(U(V))) - 35
         LET N$ = RIGHT$(B$(U(V)), N)
         LET GRA$ = MID$(B$(U(V)), 34, 2)
         LET HR$ = MID$(B$(U(V)), 31, 3)
         LET I = (INT(B(V) / 30 * 1000 + .5)) / 10
         PRINT #3, H$; " "; N$; TAB(35); GRA$; "    "; HR$; "            "; USING "###.#%"; I
      NEXT V
END SELECT

PrintEndOutput:
FOR Z = 1 TO (11 - V)
   PRINT #3, H$ + "   No Match Available"
NEXT Z

END SUB
