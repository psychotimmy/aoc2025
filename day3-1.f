      PROGRAM DAY3P1
C
      INTEGER JOLTS(128),I,CALCJOLTS,BANKSIZE,BSIZE
      INTEGER*8 TOTAL
C
   10 FORMAT(A)
   20 FORMAT(128I1)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2025 day 3, part 1"
      WRITE(*,10)" "
C     Sum of joltage across the battery banks
      TOTAL=0
C     Read the puzzle input from the file
      OPEN(10,FILE="day3in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
   50 CONTINUE
C     Read a bank of up to 127 batteries - a zero stored marks EOL
      READ(10,FMT=20,ERR=100,END=100) (JOLTS(I),I=1,128)
C     Determine the size of the battery bank
      BSIZE=BANKSIZE(JOLTS,1,128)
C     Calculate the maximum joltage from this bank
      TOTAL=TOTAL+INT8(CALCJOLTS(JOLTS,1,BSIZE))
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C
      WRITE(*,10)" "
      WRITE(*,30)"Jolts delivered by the battery banks is ",TOTAL
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
      FUNCTION CALCJOLTS(JOLTS,START,FINISH)
      INTEGER CALCJOLTS,JOLTS(*),START,FINISH
      INTEGER MAXT,MAXU,TPOS,L1
C     Find the largest 10s digit possible (ignore last battery)
      MAXT=0
      DO L1=START,FINISH-1
        IF (JOLTS(L1).GT.MAXT) THEN
          MAXT=JOLTS(L1)
          TPOS=L1
        ENDIF
      ENDDO
C     Find the largest units digit possible (from the position
C     of the maximum 10s digit plus 1)
      MAXU=0
      DO L1=TPOS+1,FINISH
        IF (JOLTS(L1).GT.MAXU) THEN
          MAXU=JOLTS(L1)
        ENDIF
      ENDDO
C     Return the value found
      CALCJOLTS=MAXT*10+MAXU
      RETURN
      END
