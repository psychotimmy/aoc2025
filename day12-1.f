      PROGRAM DAY12P1
C
      CHARACTER*24 PUZZLE
      INTEGER TOTAL,L1
      CHARACTER*2  X,Y,P(6)
      INTEGER IX,IY,IAREA
      INTEGER IP(6),IPT,PRESENTS
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(I6,A)
   40 FORMAT(A,I8,A,I8)
C
      WRITE(*,10)"Advent of Code 2025 day 12, part 1"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day12in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
   50 CONTINUE
C
C     Read each input line - throw away until we see a line
C     with a 'x' as all presents are 3x3 = 9 units at maximum
C     extent (assumption - last day is 'easy', and in any 
C     event it will give me an upper bound of failed groups of 
C     presents to refine. The ones that pass this test will always
C     work, regardless of how neatly they're packed!) 
C
      READ(10,FMT=20,ERR=100,END=100) PUZZLE
      IF (PUZZLE(3:3).NE.'x') GOTO 50
      WRITE(*,*)PUZZLE
C     The sides of each area are all 2 digits long!
      X=PUZZLE(1:2)
      Y=PUZZLE(4:5)
C     Area of region is simple to calculate = X*Y
      READ(X,*)IX
      READ(Y,*)IY
      IAREA=IX*IY
C     There are always 6 presents, each takes up 9 units of space
C     Again, input is very nicely formatted as 2 digit numbers ...
      P(1)=PUZZLE(8:9)
      P(2)=PUZZLE(11:12)
      P(3)=PUZZLE(14:15)
      P(4)=PUZZLE(17:18)
      P(5)=PUZZLE(20:21)
      P(6)=PUZZLE(23:24)
      IPT=0
      DO L1=1,6
        READ(P(L1),*)IP(L1)
        IPT=IPT+IP(L1)
      ENDDO
      PRESENTS=9*IPT
C     We now have both the area of the region and the maximum
C     space required by the presents ...
      WRITE(*,40)"Region area is",IAREA," presents require",PRESENTS
      IF (IAREA.GE.PRESENTS) TOTAL=TOTAL+1
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      WRITE(*,*)""
      WRITE(*,30)TOTAL," groups of presents pack correctly"
C
C     ... and ho,ho,ho, this simple-minded solution works for my
C     puzzle input. Happy Christmas!
C
      END
