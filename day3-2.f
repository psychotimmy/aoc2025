      PROGRAM DAY3P2
C
      INTEGER JOLTS(128),I,BANKSIZE,BSIZE
      INTEGER*8 CALCJOLTS,TOTAL2,TOTAL12
C
   10 FORMAT(A)
   20 FORMAT(128I1)
   30 FORMAT(A,I24)
C
      WRITE(*,10)"Advent of Code 2025 day 3, parts 1 and 2"
      WRITE(*,10)" "
C     Sum of joltage across the battery banks
      TOTAL2=0
      TOTAL12=0
C     Read the puzzle input from the file
      OPEN(10,FILE="day3in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
   50 CONTINUE
C     Read a bank of up to 127 batteries - a zero stored marks EOL
      READ(10,FMT=20,ERR=100,END=100) (JOLTS(I),I=1,128)
C     Determine the size of the battery bank
      BSIZE=BANKSIZE(JOLTS,1,128)
C     Calculate the maximum joltage from this bank for N batteries
      TOTAL2=TOTAL2+CALCJOLTS(JOLTS,1,BSIZE,2)
      TOTAL12=TOTAL12+CALCJOLTS(JOLTS,1,BSIZE,12)
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C
      WRITE(*,30)"Jolts from battery banks with 2 on is ",TOTAL2
      WRITE(*,30)"Jolts from battery banks with 12 on is",TOTAL12
      END
C
      FUNCTION BANKSIZE(JOLTS,START,FINISH)
      INTEGER BANKSIZE,JOLTS(*),START,FINISH
C
      INTEGER L1
C
C     Work out number of batteries in the bank by finding the first 0
C
      DO L1=START,FINISH
        IF (JOLTS(L1).EQ.0) THEN
          BANKSIZE=L1-1
          GOTO 10
        ENDIF
      ENDDO
   10 CONTINUE
      RETURN
      END
C
      FUNCTION CALCJOLTS(JOLTS,START,FINISH,NBATTS)
      INTEGER*8 CALCJOLTS
      INTEGER JOLTS(*),START,FINISH,NBATTS
      INTEGER MAXDIGIT,DIGITPOS,L1,L2
C     Find the largest NBATTS digit possible (ignore last NBATTS-L1
C     batteries each time) for the battery bank - first found wins
      DIGITPOS=START-1
C     Note DIGITPOS will become the same as START the first time
C     around the L2 loop
      CALCJOLTS=0
      DO L1=1,NBATTS
        MAXDIGIT=0
        DO L2=DIGITPOS+1,FINISH-(NBATTS-L1)
          IF (JOLTS(L2).GT.MAXDIGIT) THEN
            MAXDIGIT=JOLTS(L2)
            DIGITPOS=L2
          ENDIF
        ENDDO
C     Need to do integer*8 conversions for the integer variables
        CALCJOLTS=CALCJOLTS+
     +            INT8(JOLTS(DIGITPOS))*(10**(INT8(NBATTS-L1)))
C       WRITE(*,*)JOLTS(DIGITPOS),CALCJOLTS
      ENDDO
C     Return CALCJOLTS
      RETURN
      END
