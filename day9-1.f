      PROGRAM DAY9P1
      INTEGER NC
      PARAMETER (NC=500)
C
      COMPLEX COORDS(NC)
      INTEGER*8 RAREA,RECT,T1
      INTEGER NUM,X,Y
C
   10 FORMAT(A)
   20 FORMAT(A,I16)
   30 FORMAT(A,4F7.0)
C
      WRITE(*,10)"Advent of Code 2025 day 9, part 1"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day9in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      NUM=0
   50 CONTINUE
C     Read each input line - list directed i/o
      READ(10,FMT=*,ERR=100,END=100) X,Y
      NUM=NUM+1
      COORDS(NUM)=CMPLX(X,Y)
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      T1=0
      DO L1=1,NUM-1
        DO L2=L1+1,NUM
          RECT=RAREA(COORDS(L1),COORDS(L2))
          IF (RECT.GT.T1) THEN 
            T1=RECT
C           WRITE(*,20)"New largest rectangle area ",T1
C           WRITE(*,30)"at ",COORDS(L1),COORDS(L2)
          ENDIF
        ENDDO
      ENDDO
      WRITE(*,20)"Largest possible rectangle has area ",T1
      END
C
      INTEGER*8 FUNCTION RAREA(P1,P2)
      COMPLEX P1,P2
C
C     Returns the area of a rectangle with corners P1,P2
C     where the area of the rectangle INCLUDES the boundary
C
      INTEGER*8 I,J
C
      I=IABS(INT(REAL(P1))-INT(REAL(P2)))
      J=IABS(INT(AIMAG(P1))-INT(AIMAG(P2)))
      RAREA=(I+1)*(J+1)
      RETURN
      END
