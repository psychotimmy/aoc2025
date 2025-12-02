      PROGRAM DAY2P1
C
      CHARACTER*512 PUZZLE
      INTEGER*8 CHECKRANGE,RVALS(100),RNO,TOTAL
      INTEGER I
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2025 day 2, part 1"
      WRITE(*,10)" "
C     Sum of invalid product IDs
      TOTAL=0
C     Read the puzzle input from the file (one line assumed)
      OPEN(10,FILE="day2in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
   50 CONTINUE
      READ(10,FMT=20,ERR=100,END=100) PUZZLE
  100 CONTINUE
      CLOSE(10)
C
C     Convert PUZZLE input to integer*8s held in RVALS array
      CALL STR2INT8ARRAY(PUZZLE,100,RVALS,RNO)
C     Check the ranges for invalid product IDs and update
C     the TOTAL
      DO I=1,RNO,2
        TOTAL=TOTAL+CHECKRANGE(RVALS(I),RVALS(I+1))
      ENDDO
C
      WRITE(*,10)" "
      WRITE(*,30)"Total of invalid product IDs is ",TOTAL
      END
C
      FUNCTION CHECKRANGE(START,FINISH)
      INTEGER*8 CHECKRANGE,START,FINISH
C
      INTEGER*8 COUNT
      INTEGER CLEN,IPOW
C
      CHECKRANGE=0
C
C     We have some work to do - brute force the answer. (In my original
C     code there were lots of additional checks before this loop - none
C     of which were needed if we do it this way!) This version of the 
C     code is slower however as the expensive maths functions are called
C     for every iteration of the loop, rather than once at the start.
C
      DO COUNT=START,FINISH
        CLEN=FLOOR(DLOG10(DBLE(COUNT)))+1
        IPOW=10**(CLEN/2)
        IF ((COUNT/IPOW).EQ.MOD(COUNT,IPOW)) THEN
          CHECKRANGE=CHECKRANGE+COUNT
          WRITE(*,*)"Invalid ID found ",COUNT
        ENDIF
      ENDDO
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

