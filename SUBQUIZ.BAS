DECLARE SUB TotalReports ()       'displays all saved reports
DECLARE SUB Report (NAME$, CAT1$, CAT2$, RIGHT1, RIGHT2)  'displays current report
DECLARE SUB GeometryFacts (RIGHT) ' category 4
DECLARE SUB SciNotation (RIGHT) 'category 3
DECLARE SUB BasicMath (RIGHT) 'category 1
DECLARE SUB Algebra (RIGHT) 'category 2
DECLARE SUB Intro (NAME$, CAT1$, CAT2$, INTR$) 'checks whether info is right
DECLARE SUB CategorySelect (CAT1, CAT2, CAT1$, CAT2$) 'users selects categories
DECLARE SUB InputName (NAME$) 'user inputs name
'erl
'2-8-99
'quiz using subroutine
'sub routines
'nam = user enters name
'categories = user chooses categories
'variables
'NAME$ = person's name
'cat1 = category 1#
'cat2 = category 2#
'cat1$ = category 1 name
'cat2$ = category 2 name
'RIGHT = number of questions answered correctly
begin:
CLS

CALL InputName(NAME$)
CALL CategorySelect(CAT1, CAT2, CAT1$, CAT2$)
CALL Intro(NAME$, CAT1$, CAT2$, INTR$)
IF INTR$ = "N" GOTO begin

SELECT CASE CAT1
   CASE 1
      CALL BasicMath(RIGHT)
   CASE 2
      CALL Algebra(RIGHT)
   CASE 3
      CALL SciNotation(RIGHT)
   CASE 4
      CALL GeometryFacts(RIGHT)
END SELECT
RIGHT1 = RIGHT
RIGHT = 0
SELECT CASE CAT2
   CASE 1
      CALL BasicMath(RIGHT)
   CASE 2
      CALL Algebra(RIGHT)
   CASE 3
      CALL SciNotation(RIGHT)
   CASE 4
      CALL GeometryFacts(RIGHT)
END SELECT
RIGHT2 = RIGHT

CALL Report(NAME$, CAT1$, CAT2$, RIGHT1, RIGHT2)

COLOR 7
END

SUB Algebra (RIGHT)
CLS

LOCATE 1, 35
COLOR 10
PRINT "Algebra"

   COLOR 15
PRINT "Solve for X."

   COLOR 15
PRINT
PRINT "1 + X = 25"
INPUT "X = "; A1
IF A1 = 24 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF A1 <> 24 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 24."
END IF

   COLOR 15
PRINT
PRINT "X - 10 + 5 = 18"
INPUT "X = "; A2
IF A2 = 23 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF A2 <> 23 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 23."
END IF

   COLOR 15
PRINT
PRINT "X * 2 - 2 = 16"
INPUT "X = "; A3
IF A3 = 9 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF A3 <> 9 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 9."
END IF

   COLOR 15
PRINT
PRINT "(10 + X) / 3 = 5"
INPUT "X = "; A4
IF A4 = 5 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF A4 <> 5 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 5."
END IF

   COLOR 15
PRINT
PRINT "((16 + 10 - X) / 5) * 2 = 8"
INPUT "X = "; A5
IF A5 = 6 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF A5 <> 6 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 6."
END IF

   COLOR 14
PRINT : PRINT : PRINT :
PRINT "Press any key to continue": WHILE INKEY$ = "": WEND

END SUB

SUB BasicMath (RIGHT)

CLS
LOCATE 1, 35
   COLOR 10
PRINT "Basic Math"

   COLOR 15
PRINT "Solve"

PRINT
INPUT "2 * 3 + 5 = "; BM1
IF BM1 = 11 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF BM1 <> 11 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 11."
END IF

   COLOR 15
PRINT
INPUT "1 + 3 + 5 + 7 + 9 = "; BM2
IF BM2 = 25 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF BM2 <> 25 THEN
      COLOR 4
   PRINT "You are wrong. The right answer was 25."
END IF

   COLOR 15
PRINT
INPUT "-2 + 12 - (-5)"; BM3
IF BM3 = 15 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF BM3 <> 15 THEN
      COLOR 4
   PRINT "Incorrect. The correct answer was 10."
END IF

   COLOR 15
PRINT
INPUT "(5 - 3) * 2 = "; BM4
IF BM4 = 4 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF BM4 <> 4 THEN
      COLOR 4
   PRINT "Incorrect. The correct answer was 10."
END IF

   COLOR 15
PRINT
INPUT "2 * 1 + 4 + 6 - 2 = "; BM5
IF BM5 = 10 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF BM5 <> 10 THEN
      COLOR 4
   PRINT "Incorrect. The correct answer was 10."
END IF

   COLOR 14
PRINT : PRINT : PRINT :
PRINT "Press any key to continue": WHILE INKEY$ = "": WEND

END SUB

SUB CategorySelect (CAT1, CAT2, CAT1$, CAT2$)


CLS
   COLOR 15
PRINT "Please choose ONLY two categories from the list below:"
   COLOR 12
PRINT TAB(5); "1 - Basic Math"
PRINT TAB(5); "2 - Algebra"
PRINT TAB(5); "3 - Scientific Notation"
PRINT TAB(5); "4 - Geometry Facts"
   COLOR 2
PRINT
INPUT "Select two [1-4] "; CAT1, CAT2
IF CAT1 = CAT2 THEN
      COLOR 4
   PRINT "You cannot take the same test twice, choose again"
   PRINT "Press any key to continue":  WHILE INKEY$ = "": WEND
   CALL CategorySelect(CAT1, CAT2, CAT1$, CAT2$)
END IF
SELECT CASE CAT1
   CASE 1
      LET CAT1$ = "Basic Math"
   CASE 2
      LET CAT1$ = "Algebra"
   CASE 3
      LET CAT1$ = "Scientific Notation"
   CASE 4
      LET CAT1$ = "Geometry Facts"
   CASE ELSE
         COLOR 4
      PRINT "Invalid choice"
      PRINT "Press any key to continue": WHILE INKEY$ = "": WEND
      CALL CategorySelect(CAT1, CAT2, CAT1$, CAT2$)
END SELECT
SELECT CASE CAT2
   CASE 1
      LET CAT2$ = "Basic Math"
   CASE 2
      LET CAT2$ = "Algebra"
   CASE 3
      LET CAT2$ = "Scientific Notation"
   CASE 4
      LET CAT2$ = "Geometry Facts"
   CASE ELSE
         COLOR 4
      PRINT "Invalid choice"
      PRINT "Press any key to continue": WHILE INKEY$ = "": WEND
      CALL CategorySelect(CAT1, CAT2, CAT1$, CAT2$)
END SELECT

END SUB

SUB GeometryFacts (RIGHT)
CLS

LOCATE 1, 35
COLOR 10
PRINT "Geometry Facts"

   COLOR 15
PRINT "Determine whether the statement is true or false. Answer with [t/f]."

PRINT
   COLOR 15
PRINT "The sum of the degrees of all angles in a triangle are equal to 180."
INPUT G1$: G1$ = UCASE$(G1$)
IF G1$ = "T" THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF G1$ <> "T" THEN
      COLOR 4
   PRINT "Wrong. The statement was true."
END IF

PRINT
   COLOR 15
PRINT "The sides of all rectangles have equal lengths."
INPUT G2$: G2$ = UCASE$(G2$)
IF G2$ = "F" THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF G2$ <> "F" THEN
      COLOR 4
   PRINT "Wrong. The statement was not correct."
END IF

PRINT
   COLOR 15
PRINT "The pythagorean theorm applies only to isoceles triangles."
INPUT G3$: G3$ = UCASE$(G3$)
IF G3$ = "F" THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF G3$ <> "F" THEN
      COLOR 4
   PRINT "Wrong. The pythagorean theorm applies only to right triangles."
END IF

PRINT
   COLOR 15
PRINT "All sqaures are similar."
INPUT G4$: G4$ = UCASE$(G4$)
IF G4$ = "T" THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF G4$ <> "T" THEN
      COLOR 4
   PRINT "Wrong. The statement was true."
END IF

PRINT
   COLOR 15
PRINT "To find the circumference of a circle you can multiply the radius by 3.14 ."
INPUT G5$: G5$ = UCASE$(G5$)
IF G5$ = "T" THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF G5$ <> "T" THEN
      COLOR 4
   PRINT "Wrong. The statement was true."
END IF




   COLOR 14
PRINT : PRINT : PRINT :
PRINT "Press any key to continue": WHILE INKEY$ = "": WEND

END SUB

SUB InputName (NAME$)

NAME$ = ""
DO UNTIL NAME$ <> ""
   CLS
      COLOR 15
   INPUT "Please enter your name"; NAME$
LOOP

END SUB

SUB Intro (NAME$, CAT1$, CAT2$, INTR$)

INTR$ = ""
DO UNTIL INTR$ = "Y" OR INTR$ = "N"
   CLS
      COLOR 15
   PRINT "Welcome, "; :
      COLOR 10
   PRINT NAME$
      COLOR 15
   PRINT "Today you will be taking a test on "; :
      COLOR 14:
   PRINT CAT1$; :
      COLOR 15
   PRINT " and "; :
      COLOR 14
   PRINT CAT2$
   PRINT
      COLOR 15
   INPUT "Is this information correct [y/n]"; INTR$
   LET INTR$ = UCASE$(INTR$)
LOOP

END SUB

SUB Report (NAME$, CAT1$, CAT2$, RIGHT1, RIGHT2)
CLS

   COLOR 3
PRINT "NAME"; TAB(20); "CATEGORY"; TAB(40); "# CORRECT"; TAB(55); "PERCENTAGE"
   COLOR 10
PRINT NAME$

   COLOR 14
PRINT TAB(20); CAT1$; :
   COLOR 15
PRINT TAB(43); RIGHT1; :
   COLOR 5
PRINT TAB(58); 100 * (RIGHT1 / 5); "%"

   COLOR 14
PRINT TAB(20); CAT2$; :
   COLOR 15
PRINT TAB(43); RIGHT2; :
   COLOR 5
PRINT TAB(58); 100 * (RIGHT2 / 5); "%"

PRINT
   COLOR 2
PRINT TAB(20); "FINAL AVERAGE"; :
PRINT TAB(43); RIGHT1 + RIGHT2; :
PRINT TAB(58); 100 * ((RIGHT1 + RIGHT2) / 10); "%"

PRINT : PRINT : PRINT : PRINT : PRINT

   COLOR 9
INPUT "Would you like to save your report [y/n] "; SAVE$
IF UCASE$(SAVE$) = "Y" THEN
   OPEN "A:\SUBQUIZ.DAT" FOR APPEND AS #1
   WRITE #1, DATE$, TIME$, NAME$, CAT1$, RIGHT1, CAT2$, RIGHT2
   CLOSE #1
END IF
INPUT "Would you like to view a total report of all the tests [y/n] "; TR$
IF UCASE$(TR$) = "Y" THEN CALL TotalReports

END SUB

SUB SciNotation (RIGHT)
CLS

LOCATE 1, 35
   COLOR 10
PRINT "Scientific Notation"

   COLOR 15
PRINT "Express in decimal form. The ^ specifies the exponent."

PRINT
   COLOR 15
INPUT "6.2 * 10^1 = "; SN1
IF SN1 = 62 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF SN1 <> 62 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 62."
END IF

PRINT
   COLOR 15
INPUT "0.2 * 10^2 = "; SN2
IF SN2 = 20 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF SN2 <> 20 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 20."
END IF

PRINT
   COLOR 15
INPUT "250 * 10^-2 = "; SN3
IF SN3 = 2.5 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF SN3 <> 2.5 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 2.5 ."
END IF

PRINT
   COLOR 15
INPUT "74 * 10^2 = "; SN4
IF SN4 = 7400 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF SN4 <> 7400 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 7400 ."
END IF

PRINT
   COLOR 15
INPUT "0.2 * 10^2 = "; SN5
IF SN5 = 20 THEN
      COLOR 10
   PRINT "Correct."
   LET RIGHT = RIGHT + 1
ELSEIF SN5 <> 20 THEN
      COLOR 4
   PRINT "Wrong. The correct answer was 20."
END IF

   COLOR 14
PRINT : PRINT : PRINT :
PRINT "Press any key to continue": WHILE INKEY$ = "": WEND

END SUB

SUB TotalReports
CLS
  
   COLOR 3
PRINT "NAME, D & T"; TAB(20); "CATEGORY"; TAB(40); "# CORRECT"; TAB(55); "PERCENTAGE"

OPEN "A:\SUBQUIZ.DAT" FOR INPUT AS #1
DO
   INPUT #1, D$, T$, NAME$, CAT1$, RIGHT1, CAT2$, RIGHT2
      COLOR 6
   PRINT "------------------------------------------------------------------"
     
      COLOR 10
   PRINT NAME$; :
      COLOR 14
   PRINT TAB(20); CAT1$; :
      COLOR 15
   PRINT TAB(43); RIGHT1; :
      COLOR 5
   PRINT TAB(58); 100 * (RIGHT1 / 5); "%"

      COLOR 11
   PRINT D$; :
      COLOR 14
   PRINT TAB(20); CAT2$; :
      COLOR 15
   PRINT TAB(43); RIGHT2; :
      COLOR 5
   PRINT TAB(58); 100 * (RIGHT2 / 5); "%"

      COLOR 11
   PRINT T$; :
      COLOR 2
   PRINT TAB(20); "FINAL AVERAGE"; :
   PRINT TAB(43); RIGHT1 + RIGHT2; :
   PRINT TAB(58); 100 * ((RIGHT1 + RIGHT2) / 10); "%"

LOOP UNTIL EOF(1): CLOSE #1
  
   COLOR 6
PRINT "------------------------------------------------------------------"

END SUB
