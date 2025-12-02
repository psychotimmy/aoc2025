      PROGRAM DAY2P2
C
      CHARACTER*512 PUZZLE
      INTEGER*8 CHECKRANGE,RVALS(100),RNO,TOTAL
      INTEGER I
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2025 day 2, part 2"
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
      CHARACTER*16 PID,SLICE
      INTEGER CLEN,IPOW,L2,L3
      INTEGER*8 PRODUCTID,TEMP
      LOGICAL MATCH
C
      CHECKRANGE=0
C
      DO PRODUCTID=START,FINISH
C       Calculate length of product id being tested
        CLEN=FLOOR(DLOG10(DBLE(PRODUCTID)))+1
C
C       Only certain sequence repeats are possible depending on the
C       length of the product id. e.g. 2,3,5,7 - sequence length 1.
C       4 - sequence lengths 1 and 2. 6 - sequence lengths 1,2 and 3 etc.
C       These happen when MOD(Product id length,seq length) is 0.
        DO L2=1,CLEN/2
          IF (MOD(CLEN,L2).EQ.0) THEN
C           We have an exact divisor for the product id length
C           Convert it and the product ids to strings for ease
C           of comparision
            WRITE(PID,'(I16)')PRODUCTID
            WRITE(SLICE,'(I16)')MOD(PRODUCTID,10**L2)
C           WRITE(*,*)PID,SLICE
C           Compare each slice with the product id. If we get
C           a match for all slices the id is valid and we should add
C           this to the total for this range and try the next id.
            DO L3=16,16-CLEN+1,-L2
              MATCH=.TRUE.
C             WRITE(*,*) PID((L3-L2+1):L3)," -- ",SLICE(16-L2+1:16)
              IF (PID((L3-L2+1):L3).NE.SLICE(16-L2+1:16)) THEN
                MATCH=.FALSE.
C               WRITE(*,*)"NO MATCH"
                GOTO 50
              ENDIF
            ENDDO
   50       CONTINUE
            IF (MATCH.EQV..TRUE.) THEN
              WRITE(*,*)"Invalid ID found ",PRODUCTID
              CHECKRANGE=CHECKRANGE+PRODUCTID
              GOTO 100
            ENDIF
          ENDIF
        ENDDO
  100 CONTINUE
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

