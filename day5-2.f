      PROGRAM DAY5P2
C
      CHARACTER*50 PUZZLE
      INTEGER*8 RANGES(400),NR(400),I8PUZZLE(2),IVALS,TOTAL
      LOGICAL READINGREDIENTS
      INTEGER L1,L2,NORA,ICHECK,NONRA,RTP
      LOGICAL CHECKRANGE
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I24)
   40 FORMAT(2I24)
   50 FORMAT(I4,A)
C
      WRITE(*,10)"Advent of Code 2025 day 5, part 2"
      WRITE(*,10)" "
C     Number of fresh ingredient IDs found
      TOTAL=0
C     Read the puzzle input from the file
      OPEN(10,FILE="day5in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Read only the ranges
      READINGREDIENTS=.FALSE.
      NORA=0
   60 CONTINUE
      READ(10,FMT=20,ERR=100,END=100) PUZZLE
C     Blank line = end of ranges input
      IF (PUZZLE(1:1).EQ." ") THEN 
        READINGREDIENTS=.TRUE.
        GOTO 100
      ENDIF
C     Populate the RANGES array. RANGES array is odd numbers
C     are the low part of the range, even the high.
      CALL STR2INT8ARRAY(PUZZLE,2,I8PUZZLE,IVALS)
C     Should always return 2 elements
      IF (IVALS.NE.2) STOP 8
C
      RANGES(NORA*2+1)=I8PUZZLE(1)
      RANGES(NORA*2+2)=I8PUZZLE(2)
C     WRITE(*,*) RANGES(NORA*2+1),
C    +           RANGES(NORA*2+2)
      NORA=NORA+1
      GOTO 60
  100 CONTINUE
      CLOSE(10)
C     We need to collapse the ranges onto as few as possible
C     RTP is the number of original RANGES left to process
C     NONRA is the number of consolidated ranges in NR
      RTP=NORA
      NONRA=0
      DO WHILE (RTP.GT.0)
C       WRITE(*,50)RTP," ranges left to process"
C
C     First unprocessed RANGE is always good - so transfer to NR
C     and mark it off (set both ends to -1) in RANGES. As soon as
C     we find one, exit this loop
C
        DO L1=1,NORA*2,2
          IF (RANGES(L1).NE.-1) THEN
            NR(NONRA*2+1)=RANGES(L1)
            NR(NONRA*2+2)=RANGES(L1+1)
            NONRA=NONRA+1
            RANGES(L1)=-1
            RANGES(L1+1)=-1
            RTP=RTP-1
C     If the last RANGES was processed - stop now.
            IF (RTP.EQ.0) GOTO 999
C     Otherwise look for supersets of NR ranges
            GOTO 150
          ENDIF
        ENDDO
C
C     Check to see if we have a superset of the range we're checking
C     in NR. If so, replace that range and start again.
C
  150   CONTINUE
C       WRITE(*,*)"Superset processing"
        DO L1=1,NORA*2,2
          DO L2=1,NONRA*2,2
            IF (RANGES(L1).NE.-1) THEN
              IF (CHECKRANGE(NR(L2),RANGES(L1),RANGES(L1+1)).AND.
     +            CHECKRANGE(NR(L2+1),RANGES(L1),RANGES(L1+1))) THEN
C               WRITE(*,*)"Superset",RANGES(L1),RANGES(L1+1),
C    +                    " covers ",NR(L2),NR(L2+1)
                NR(L2)=RANGES(L1)
                NR(L2+1)=RANGES(L1+1)
C     We've now finished with that entry in RANGES
                RANGES(L1)=-1
                RANGES(L1+1)=-1
                RTP=RTP-1
C     If the last RANGES was processed - stop now.
                IF (RTP.EQ.0) THEN 
                  GOTO 999
                ELSE
C     Start checking for supersets again
                  GOTO 150
                ENDIF
              ELSE
C               WRITE(*,*)RANGES(L1),RANGES(L1+1),
C    +                    " is NOT a superset of",NR(L2),NR(L2+1)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C     Now check to see if we can extend the RHS of any range already
C     in NR
C
  200   CONTINUE
C       WRITE(*,*)"RHS range limit processing"
        DO L1=1,NORA*2,2
          DO L2=1,NONRA*2,2
            IF (RANGES(L1).NE.-1) THEN
              IF (CHECKRANGE(RANGES(L1),NR(L2),NR(L2+1))) THEN
C               WRITE(*,*)"LHS",RANGES(L1)," is in ",NR(L2),NR(L2+1)
C     RHS of NR becomes RHS of RANGES if it's larger!
                IF (RANGES(L1+1).GT.NR(L2+1)) THEN
C                 WRITE(*,*)"RHS goes from",NR(L2+1)," to",RANGES(L1+1)
                  NR(L2+1)=RANGES(L1+1)
                ELSE
C                 WRITE(*,*)"RHS of NR NOT expanded"
                ENDIF
C     We've now finished with that entry in RANGES
                RANGES(L1)=-1
                RANGES(L1+1)=-1
                RTP=RTP-1
C     If the last RANGES was processed - stop now.
                IF (RTP.EQ.0) THEN 
                  GOTO 999
                ELSE
C     Start expanding the RHS again
                  GOTO 200
                ENDIF
              ELSE
C               WRITE(*,*)"LHS",RANGES(L1)," NOT in",NR(L2),NR(L2+1)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C     Now check to see if we can extend the LHS of any range already
C     in NR
C
  300   CONTINUE
C       WRITE(*,*)"LHS range processing"
        DO L1=1,NORA*2,2
          DO L2=1,NONRA*2,2
            IF (RANGES(L1).NE.-1) THEN
              IF (CHECKRANGE(RANGES(L1+1),NR(L2),NR(L2+1))) THEN
C               WRITE(*,*)"RHS",RANGES(L1+1)," is in ",NR(L2),NR(L2+1)
C     LHS of NR becomes LHS of RANGES if it's smaller!
                IF (RANGES(L1).LT.NR(L2)) THEN
                  NR(L2)=RANGES(L1)
C                 WRITE(*,*)"LHS goes from",NR(L2)," to",RANGES(L1)
                ELSE
C                 WRITE(*,*)"LHS of NR NOT expanded"
                ENDIF
C     We've now finished with that entry in RANGES
                RANGES(L1)=-1
                RANGES(L1+1)=-1
                RTP=RTP-1
C     If the last RANGES was processed - stop now.
                IF (RTP.EQ.0) THEN
                  GOTO 999
                ELSE
C     Start expanding the LHS again
                  GOTO 300
                ENDIF
              ELSE
C               WRITE(*,*)"RHS",RANGES(L1+1)," NOT in",NR(L2),NR(L2+1)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C 
C     Finally find all the RANGES contained in NR and mark them
C     as processed in RANGES
C
        DO L1=1,NORA*2,2
          DO L2=1,NONRA*2,2
            IF (RANGES(L1).NE.-1) THEN
              IF (CHECKRANGE(RANGES(L1),NR(L2),NR(L2+1))
     +            .AND. CHECKRANGE(RANGES(L1+1),NR(L2),NR(L2+1))) THEN
C               WRITE(*,*)RANGES(L1),RANGES(L1+1)," fully contained"
                RANGES(L1)=-1
                RANGES(L1+1)=-1
                RTP=RTP-1
C     If the last RANGES was processed - stop now.
                IF (RTP.EQ.0) GOTO 999
              ELSE
C               WRITE(*,*)RANGES(L1),RANGES(L1+1)," NOT fully contained"
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
      ENDDO
 999  CONTINUE
C     WRITE(*,10)" "
C     WRITE(*,10)"Processed ranges are"
C     WRITE(*,10)" "
      DO L1=1,NONRA*2,2
C       WRITE(*,40)NR(L1),NR(L1+1)
        TOTAL=TOTAL+NR(L1+1)-NR(L1)+1
      ENDDO
      WRITE(*,10)" "
      WRITE(*,30)"Total number of fresh ingredient IDs is ",TOTAL
      END
C
      FUNCTION CHECKRANGE(VALUE,START,FINISH)
      LOGICAL CHECKRANGE
      INTEGER*8 VALUE,START,FINISH
C
      CHECKRANGE=.FALSE.
      IF ((VALUE.GE.START).AND.(VALUE.LE.FINISH)) THEN
        CHECKRANGE=.TRUE.
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
