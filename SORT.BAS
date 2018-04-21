'erl
'4-20-99
'Bubble sorting and searching array for a captured prisoner
'Prompts user for input on which prisoner to find
DECLARE SUB SelectAndSort ()
DECLARE SUB FindID ()
DECLARE SUB FindCell ()
DECLARE SUB FindShip ()
DECLARE SUB FindPrisoner ()
DATA "Kenobi", "Falcon", "328", "Spock", "Enterprise", "562"
DATA "Yoda", "None", "122", "Mudd", "Pleasure Dome", "222"
DATA "Khan", "Botany Bay", "009", "Jetson", "Astrofly", "468"
DATA "Rogers", "Galaxy 2", "727", "Koenig", "Alpher", "999"
DATA "Adama", "Galactic", "987", "Who", "Tardis", "585"
DIM SHARED Prisoner$(1 TO 10)
DIM SHARED Ship$(1 TO 10)
DIM SHARED Cell$(1 TO 10)
DIM SHARED Identity$(1 TO 10)
DIM SHARED Sorting$(1 TO 10)
RANDOMIZE TIMER

FOR X = 1 TO 10     ' **** gets data for each prisoner
   READ Prisoner$(X), Ship$(X), Cell$(X)
   FOR Y = 1 TO INT(RND * 5) + 5
      Identity$(X) = Identity$(X) + CHR$(INT(RND * 223 + 32))
   NEXT Y
NEXT X

DO
   CALL SelectAndSort
LOOP

COLOR 7
END

SUB FindCell '*****************Sub to search by Cell #***********************

CLS
COLOR 10       ' **** user inputs what cell to search for
INPUT "What cell are you looking for"; Request$
URequest$ = UCASE$(Request$)

Up = 10
Lo = 1
Middle = INT((Up + Lo) / 2)

DO WHILE (Lo <= Up) AND (URequest$ <> Test$) ' **** binary search
   Test$ = UCASE$(Cell$(Middle))
   IF URequest$ < Test$ THEN
      Up = Middle - 1
   ELSEIF URequest$ > Test$ THEN
      Lo = Middle + 1
   END IF
   Middle = INT((Up + Lo) / 2)
LOOP
IF Lo > Up THEN                 ' **** if cell is not found in search
   COLOR 12
   PRINT : PRINT : PRINT
   PRINT "The cell ";
   COLOR 15
   PRINT Request$;
   COLOR 12
   PRINT " is empty."
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
ELSE                      ' **** if cell is found in search
   PRINT : PRINT : PRINT : PRINT
   COLOR 14
   PRINT "Cell"; TAB(15); "Name"; TAB(29); "Ship"; TAB(40); "Klingon ID"
   COLOR 6
   PRINT "®è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è¯"
   COLOR 15
   PRINT Cell$(Middle); TAB(15); Prisoner$(Middle); TAB(29); Ship$(Middle); TAB(40); Identity$(Middle)
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
END IF

END SUB

SUB FindID ' ******************Sub to search for ID string*******************

DO                 ' **** user inputs request
   CLS
   COLOR 10
   INPUT "What Klingon ID string do you request"; Request$
   URequest$ = UCASE$(Request$)
   X = VAL(LEFT$(Request$, 1))
LOOP WHILE ((X > 0) AND (X <= 9)) OR (LEFT$(Request$, 1)) = "0"

Up = 10
Lo = 1
Middle = INT((Up + Lo) / 2)

DO WHILE (Lo <= Up) AND (URequest$ <> Test$) ' **** binary search for ID
   Test$ = UCASE$(Identity$(Middle))
   IF URequest$ < Test$ THEN
      Up = Middle - 1
   ELSEIF URequest$ > Test$ THEN
      Lo = Middle + 1
   END IF
   Middle = INT((Up + Lo) / 2)
LOOP
IF Lo > Up THEN          ' **** if ID string is not found
   COLOR 12
   PRINT : PRINT : PRINT
   PRINT "The prisoner with the Klingon ID string of ";
   COLOR 15
   PRINT Request$;
   COLOR 12
   PRINT " could not be found."
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
ELSE                          ' **** if ID string is found
   PRINT : PRINT : PRINT : PRINT
   COLOR 14
   PRINT "Klingon ID"; TAB(15); "Ship"; TAB(29); "Cell"; TAB(40); "Name"
   COLOR 6
   PRINT "®è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è¯"
   COLOR 15
   PRINT Identity$(Middle); TAB(15); Ship$(Middle); TAB(30); Cell$(Middle); TAB(40); Prisoner$(Middle)
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
END IF

END SUB

SUB FindPrisoner ' *************Sub to search for prisoner*******************

DO         ' **** User inputs request string
   CLS
   COLOR 10
   INPUT "What prisoner are you looking for"; Request$
   URequest$ = UCASE$(Request$)
   X = VAL(LEFT$(Request$, 1))
LOOP WHILE ((X > 0) AND (X <= 9)) OR (LEFT$(Request$, 1)) = "0"

Up = 10
Lo = 1
Middle = INT((Up + Lo) / 2)

DO WHILE (Lo <= Up) AND (URequest$ <> Test$) ' **** binary search for prisoner
   Test$ = UCASE$(Prisoner$(Middle))
   IF URequest$ < Test$ THEN
      Up = Middle - 1
   ELSEIF URequest$ > Test$ THEN
      Lo = Middle + 1
   END IF
   Middle = INT((Up + Lo) / 2)
LOOP
IF Lo > Up THEN          ' **** if prisoner is not found
   COLOR 12
   PRINT : PRINT : PRINT
   PRINT "The prisoner ";
   COLOR 15
   PRINT Request$;
   COLOR 12
   PRINT " could not be found."
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
ELSE                          ' **** if prisoner is found
   PRINT : PRINT : PRINT : PRINT
   COLOR 14
   PRINT "Name"; TAB(15); "Ship"; TAB(29); "Cell"; TAB(40); "Klingon ID"
   COLOR 6
   PRINT "®è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è¯"
   COLOR 15
   PRINT Prisoner$(Middle); TAB(15); Ship$(Middle); TAB(30); Cell$(Middle); TAB(40); Identity$(Middle)
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
END IF

END SUB

SUB FindShip '*******************Sub to search for ship**********************

DO                   ' **** user inputs request for ship
   CLS
   COLOR 10
   INPUT "What ship are you looking for"; Request$
   URequest$ = UCASE$(Request$)
   X = VAL(LEFT$(Request$, 1))
LOOP WHILE ((X > 0) AND (X <= 9)) OR ((LEFT$(Request$, 1)) = "0")

Up = 10
Lo = 1
Middle = INT((Up + Lo) / 2)

DO WHILE (Lo <= Up) AND (URequest$ <> Test$) ' **** binary search
   Test$ = UCASE$(Ship$(Middle))
   IF URequest$ < Test$ THEN
      Up = Middle - 1
   ELSEIF URequest$ > Test$ THEN
      Lo = Middle + 1
   END IF
   Middle = INT((Up + Lo) / 2)
LOOP
IF Lo > Up THEN      ' **** if ship is not found
   COLOR 12
   PRINT : PRINT : PRINT
   PRINT "The ship ";
   COLOR 15
   PRINT Request$;
   COLOR 12
   PRINT " could not be found."
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
ELSE                             ' **** if ship is found
   PRINT : PRINT : PRINT : PRINT
   COLOR 14
   PRINT "Ship"; TAB(15); "Prisoner"; TAB(29); "Cell"; TAB(40); "Klingon ID"
   COLOR 6
   PRINT "®è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è+è¯"
   COLOR 15
   PRINT Ship$(Middle); TAB(15); Prisoner$(Middle); TAB(30); Cell$(Middle); TAB(40); Identity$(Middle)
   COLOR 7
   PRINT : PRINT : PRINT : PRINT : PRINT
   PRINT "Press any key to continue"
   WHILE INKEY$ = "": WEND
END IF

END SUB

SUB SelectAndSort '*************Sub to select and sort category**************

CLS        ' **** prints selections for search
COLOR 2
PRINT "Which category would you like to search?"
COLOR 4
PRINT "   1 ¯ Name"
PRINT "   2 ¯ Ship"
PRINT "   3 ¯ Cell"
PRINT "   4 ¯ Klingon ID String"
PRINT
COLOR 11
PRINT "Enter a category [1-4].  ";
COLOR 12
PRINT "Press [q] to end."
COLOR 14
DO UNTIL (Cat >= 1) AND (Cat <= 4)    ' **** user inputs selection
   Cat$ = INKEY$
   Cat = VAL(Cat$)
   IF UCASE$(Cat$) = "Q" THEN
      COLOR 7
      END
   END IF
LOOP

FOR X = 1 TO 10               ' **** sets the sorting to selected thing
   IF Cat = 1 THEN Sorting$(X) = Prisoner$(X)
   IF Cat = 2 THEN Sorting$(X) = Ship$(X)
   IF Cat = 3 THEN Sorting$(X) = Cell$(X)
   IF Cat = 4 THEN Sorting$(X) = Identity$(X)
NEXT X

FOR i = 1 TO 9                ' **** sorts item by selected thing
   FOR j = i + 1 TO 10
      IF Sorting$(i) > Sorting$(j) THEN
         SWAP Sorting$(j), Sorting$(i)
         SWAP Prisoner$(j), Prisoner$(i)
         SWAP Ship$(j), Ship$(i)
         SWAP Cell$(j), Cell$(i)
         SWAP Identity$(j), Identity$(i)
      END IF
   NEXT j
NEXT i

IF Cat = 1 THEN FindPrisoner     ' **** calls sub for selected search
IF Cat = 2 THEN FindShip
IF Cat = 3 THEN FindCell
IF Cat = 4 THEN FindID

END SUB
