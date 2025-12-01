      PROGRAM DAY1P1
C
      CHARACTER*1 ROTATION
      INTEGER VALUE,NEWVAL,TOTAL
C
   10 FORMAT(A)
C     Format statement 20 is good for rotations up to 9,999
   20 FORMAT(A1,I4)
   30 FORMAT(A,I5,A)
C
      WRITE(*,10)"Advent of Code 2025 day 1, part 1"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day1in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Starting value for the dial is position 50
      VALUE=50
C     Count the number of times the dial reads 0
      TOTAL=0
   50 CONTINUE
C     Read each input line
      READ(10,FMT=20,ERR=100,END=100) ROTATION,NEWVAL
C     Don't need to worry about MOD returning negative values as 
C     we're only looking for when the dial reads exactly 0.
      IF (ROTATION.EQ.'L') THEN
        VALUE=MOD(VALUE-NEWVAL,100)
      ELSE
        VALUE=MOD(VALUE+NEWVAL,100)
      ENDIF
      IF (VALUE.EQ.0) THEN
        TOTAL=TOTAL+1
      ENDIF
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      WRITE(*,30)"Zero is indicated ",TOTAL," times"
      END
