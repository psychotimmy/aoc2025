      PROGRAM DAY5P1
C
      CHARACTER*50 PUZZLE
      INTEGER*8 RANGES(400),INGREDIENTS(1100),I8PUZZLE(2),IVALS
      LOGICAL READINGREDIENTS
      INTEGER TOTAL,L1,L2,NORANGES,NOINGREDIENTS,ICHECK
      INTEGER CHECKRANGE
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2025 day 5, part 1"
      WRITE(*,10)" "
C     Number of fresh ingredients found
      TOTAL=0
C     Read the puzzle input from the file
      OPEN(10,FILE="day5in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Read the ranges first
      READINGREDIENTS=.FALSE.
      NORANGES=0
      NOINGREDIENTS=0
   50 CONTINUE
      READ(10,FMT=20,ERR=100,END=100) PUZZLE
C     Skip over the blank line and read the ingredients
      IF (PUZZLE(1:1).EQ." ") THEN 
        READINGREDIENTS=.TRUE.
        GOTO 50
      ENDIF
C     Populate the appropriate array. RANGES array is odd numbers
C     are the low part of the range, even the high. INGREDIENTS
C     array is odd numbers = ingredient id, next even is 1 if the
C     ingredient is spoiled (0 to start with before ranges checked).
      IF (READINGREDIENTS.EQV..TRUE.) THEN
        CALL STR2INT8ARRAY(PUZZLE,1,I8PUZZLE,IVALS)
C       Should always return 1 element
        IF (IVALS.NE.1) STOP 7
        NOINGREDIENTS=NOINGREDIENTS+1
        INGREDIENTS(NOINGREDIENTS)=I8PUZZLE(1)
C       WRITE(*,*) INGREDIENTS(NOINGREDIENTS),
      ELSE
        CALL STR2INT8ARRAY(PUZZLE,2,I8PUZZLE,IVALS)
C       Should always return 2 elements
        IF (IVALS.NE.2) STOP 8
        RANGES(NORANGES*2+1)=I8PUZZLE(1)
        RANGES(NORANGES*2+2)=I8PUZZLE(2)
C       WRITE(*,*) RANGES(NORANGES*2+1),
C    +             RANGES(NORANGES*2+2)
        NORANGES=NORANGES+1
      ENDIF
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C
C     Check ingredient against each range, stop when it is
C     either fresh or we've checked all the ranges (= spoiled)
C
      DO L1=1,NOINGREDIENTS
        DO L2=1,NORANGES*2,2
          ICHECK=CHECKRANGE(INGREDIENTS(L1),RANGES(L2),RANGES(L2+1))
          IF (ICHECK.EQ.1) THEN
            TOTAL=TOTAL+1
C           Don't need to check any further ranges, go to next
C           ingredient to check
            GOTO 200
          ENDIF
        ENDDO
  200   CONTINUE
      ENDDO
      WRITE(*,30)"Total number of fresh ingredients is ",TOTAL
      END
C
      FUNCTION CHECKRANGE(VALUE,START,FINISH)
      INTEGER CHECKRANGE
      INTEGER*8 VALUE,START,FINISH
C
      CHECKRANGE=0
      IF ((VALUE.GE.START).AND.(VALUE.LE.FINISH)) THEN
        CHECKRANGE=1
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE STR2INT8ARRAY(STR,MAXL,LIST,ELEMENTS)
      CHARACTER*(*) STR
      INTEGER MAXL
      INTEGER*8 LIST(*), ELEMENTS
C
C     Separates a list of MAXL integers stored in STR into the
C     array LIST. The number of consecutive elements read from
C     the start of the list is returned in ELEMENTS. Format of
C     STR needs to be blank separated, so any other characters
C     in STR are replaced by blanks before processing. This 
C     version only removes - and , - this can be changed in the
C     DO 20 loop.
C
C     This method assumes STR does NOT start with a blank when
C     passed to this subroutine, as blank in position 1 = end of
C     string of values to parse OR if we're going to overflow
C     the integer array (ELEMENTS values found = MAXL)
C
C     Note that STR is destroyed by this subroutine!
C
      INTEGER IDX
C     Format assumes values in range -999,999,999,999,999 .. 9,999,999,999,999,999
    5 FORMAT(I16)
C     Zero the return array
      DO 10 IDX=1,MAXL
        LIST(IDX)=0
   10 CONTINUE
C     Remove any separator characters - and ,
      DO 20 IDX=1,LEN(STR)
        IF (STR(IDX:IDX).EQ.',') THEN
          STR(IDX:IDX)=' '
        ENDIF
        IF (STR(IDX:IDX).EQ.'-') THEN
          STR(IDX:IDX)=' '
        ENDIF
   20 CONTINUE 
C
      ELEMENTS=0
  100 CONTINUE
      IDX=INDEX(STR,' ')
      IF ((IDX.EQ.1).OR.(ELEMENTS.EQ.MAXL)) GOTO 999
C     We have an integer value, put it in the next array element
      ELEMENTS=ELEMENTS+1
      READ(STR(1:IDX-1),5) LIST(ELEMENTS)
C     And truncate the front of the string for the next value
      STR(1:)=STR(IDX+1:)
      GOTO 100
  999 CONTINUE
      RETURN
      END
