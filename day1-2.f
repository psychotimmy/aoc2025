      PROGRAM DAY1P2
C
      CHARACTER*1 ROTATION
      INTEGER VALUE,NEWVAL,TEMP,TOTAL
C
   10 FORMAT(A)
C     Format statement 20 is good for rotations up to 9,999
   20 FORMAT(A1,I4)
   30 FORMAT(A,I5,A)
C
      WRITE(*,10)"Advent of Code 2025 day 1, part 2"
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
C     Divide by 100 to get the number of complete spins of the dial
      WRITE(*,*)"Processing ",ROTATION,NEWVAL
      TOTAL=TOTAL+NEWVAL/100
C     Get the remainder via MOD to test to see if there's another pass
C     through 0 or landing exactly on 0.
      NEWVAL=MOD(NEWVAL,100)
      WRITE(*,*)"Total 0s now",TOTAL," so processing ",ROTATION,NEWVAL
      TEMP=VALUE
C
      IF (ROTATION.EQ.'L') THEN
        VALUE=MOD(VALUE-NEWVAL,100)
C       If the value has gone negative then correct the dial
        IF (VALUE.LT.0) THEN
          VALUE=100+VALUE
        ENDIF
C       If the dial is now at a larger number than before AND we didn't
C       start at 0, we've clicked through 0 once more
        IF ((VALUE.GT.TEMP).AND.(TEMP.NE.0)) THEN
          TOTAL=TOTAL+1
          WRITE(*,*)"Total 0s after L rotation",TOTAL
        ENDIF
      ELSE
        VALUE=MOD(VALUE+NEWVAL,100)
        IF ((VALUE.LT.TEMP).AND.(VALUE.NE.0)) THEN
          TOTAL=TOTAL+1
          WRITE(*,*)"Total 0s after R rotation",TOTAL
        ENDIF
      ENDIF
      WRITE(*,*)"Dial moved from",TEMP," to",VALUE
C     Finally, if the dial is at 0 and it did not start at 0, add 1
C     to the total.
      IF ((VALUE.EQ.0).AND.(TEMP.NE.0)) THEN
        TOTAL=TOTAL+1
        WRITE(*,*)"Total 0s after moving and landing on 0",TOTAL
      ENDIF
      IF ((TEMP.EQ.0).AND.(VALUE.EQ.0)) THEN
        WRITE(*,*)"Moved dial from 0 to 0 - no new 0 to add!!"
      ENDIF
      WRITE(*,*)"---------------------------------"
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      WRITE(*,30)"Zero is clicked",TOTAL," times"
      END
