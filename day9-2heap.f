      PROGRAM DAY9P2
      INTEGER NC,MAXRECTS
      PARAMETER (NC=496)
      PARAMETER (MAXRECTS=122760)
C
      COMPLEX COORDS(NC)
      INTEGER VERT(NC/2,3)
      INTEGER HORZ(NC/2,3)
      INTEGER*8 RECTS(MAXRECTS,5)
      INTEGER*8 RAREA,RECT,T1
      INTEGER NUM,X,Y,L1,L2,N,NRECTS
      LOGICAL CHECK2RED,CHECKRECT,FOUND
C
   10 FORMAT(A)
   20 FORMAT(A,I16)
   30 FORMAT(A,4F7.0)
   40 FORMAT(I16,4I6)
C
      WRITE(*,10)"Advent of Code 2025 day 9, parts 1 and 2"
C     WRITE(*,10)" "
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
C
C     ASSUMPTION - there EXACTLY 2 or 0 red squares on each row
C                  and column of the grid
C
      IF (.NOT.(CHECK2RED(COORDS,NUM))) THEN
        WRITE(*,10)"EXACTLY 2 or 0 assumption not met"
        STOP 8
      ENDIF
C
C     Create two lists of horizontal and vertical edges of size
C     NUM/2
C
      CALL CREATELISTS(COORDS,NUM,VERT,HORZ)
C
C     Create a list of all possible rectangles
C
      NRECTS=0
      DO L1=1,NUM-1
        DO L2=L1+1,NUM
          NRECTS=NRECTS+1
          RECT=RAREA(COORDS(L1),COORDS(L2))
          RECTS(NRECTS,1)=RECT
          RECTS(NRECTS,2)=INT(REAL(COORDS(L1)))
          RECTS(NRECTS,3)=INT(AIMAG(COORDS(L1)))
          RECTS(NRECTS,4)=INT(REAL(COORDS(L2)))
          RECTS(NRECTS,5)=INT(AIMAG(COORDS(L2)))
        ENDDO
      ENDDO
C
C     Sort the rectangles by area, largest first
C
      CALL HEAPSORT(RECTS,MAXRECTS,NRECTS)
C
C     This is the part 1 result, but slower!
C
      WRITE(*,*)""
      WRITE(*,20)"Largest possible rectangle has area ",RECTS(1,1)
C
C     Work through the list until we find a rectangle that fits 
C     inside the points boundary
C
      FOUND=.FALSE.
      L1=0
      N=INT(NUM/2)
      DO WHILE (.NOT.(FOUND).AND.(L1.LT.NRECTS))
        L1=L1+1
C       WRITE(*,*)""
C       WRITE(*,*)"Checking rectangle ",L1
C       WRITE(*,*)""
        FOUND=CHECKRECT(RECTS(L1,2),RECTS(L1,3),
     +                  RECTS(L1,4),RECTS(L1,5),
     +                  VERT,HORZ,N,COORDS,NUM)
      ENDDO 
      WRITE(*,20)
     +  "Largest rectangle inside boundary has area ",RECTS(L1,1)
      END
C
      SUBROUTINE CREATELISTS(COORDS,N,V,H)
      COMPLEX COORDS(*)
      INTEGER N,V(N/2,3),H(N/2,3)
      INTEGER CHECKX(N),CHECKY(N),TEMP(N),L1,L2
      INTEGER IDX
C
   10 FORMAT(3I6)
C
      DO L1=1,N
        CHECKX(L1)=INT(REAL(COORDS(L1)))
        CHECKY(L1)=INT(AIMAG(COORDS(L1)))
      ENDDO
C     Create list of vertical edges V
      TEMP(1)=CHECKX(1)
      IDX=1
C     Store the first X value and it's Y value in list V
      V(IDX,1)=CHECKX(1)
      V(IDX,2)=CHECKY(1)
      DO L1=2,N
        DO L2=1,IDX
          IF (TEMP(L2).EQ.CHECKX(L1)) THEN
C           We've seen this X value once, negate it
            TEMP(L2)=-TEMP(L2)
C           Store its Y value in column 3 of list V if larger,
C           otherwise swap with column 2
            IF (CHECKY(L1).GT.V(L2,2)) THEN
              V(L2,3)=CHECKY(L1)
            ELSE
              V(L2,3)=V(L2,2)
              V(L2,2)=CHECKY(L1)
            ENDIF
            GOTO 100
          ENDIF
        ENDDO
        IDX=IDX+1
        TEMP(IDX)=CHECKX(L1)
C       Store the first X value and it's Y value in list V
        V(IDX,1)=CHECKX(L1)
        V(IDX,2)=CHECKY(L1)
  100   CONTINUE
      ENDDO
C     Create list of horizontal edges H
      TEMP(1)=CHECKY(1)
      IDX=1
C     Store the first Y value and it's X value in list H
      H(IDX,1)=CHECKY(1)
      H(IDX,2)=CHECKX(1)
      DO L1=2,N
        DO L2=1,IDX
          IF (TEMP(L2).EQ.CHECKY(L1)) THEN
C           We've seen this Y value once, negate it
            TEMP(L2)=-TEMP(L2)
C           Store its X value in column 3 of list H if larger,
C           otherwise swap with column 2
            IF (CHECKX(L1).GT.H(L2,2)) THEN
              H(L2,3)=CHECKX(L1)
            ELSE
              H(L2,3)=H(L2,2)
              H(L2,2)=CHECKX(L1)
            ENDIF
            GOTO 200
          ENDIF
        ENDDO
        IDX=IDX+1
        TEMP(IDX)=CHECKY(L1)
C       Store the first Y value and it's X value in list H
        H(IDX,1)=CHECKY(L1)
        H(IDX,2)=CHECKX(L1)
  200   CONTINUE
      ENDDO
C     WRITE(*,*)"Verticals (X,Y1,Y2)"
C     DO L1=1,IDX
C       WRITE(*,10)V(L1,1),V(L1,2),V(L1,3)
C     ENDDO
C     WRITE(*,*)" "
C     WRITE(*,*)"Horizontals (Y,X1,X2)"
C     DO L1=1,IDX
C       WRITE(*,10)H(L1,1),H(L1,2),H(L1,3)
C     ENDDO
      RETURN
      END
C
      LOGICAL FUNCTION CHECK2RED(COORDS,NUM)
C     Check that there are exactly 2 matching X values
C     and Y values in the list of COORDS
      COMPLEX COORDS(*)
      INTEGER NUM
C
      INTEGER CHECKX(NUM),CHECKY(NUM),TEMP(NUM),L1,L2
      INTEGER IDX,JDX
C
      DO L1=1,NUM
        CHECKX(L1)=INT(REAL(COORDS(L1)))
        CHECKY(L1)=INT(AIMAG(COORDS(L1)))
      ENDDO
C
      TEMP(1)=CHECKX(1)
      IDX=1
      DO L1=2,NUM
        DO L2=1,IDX
          IF (TEMP(L2).EQ.CHECKX(L1)) THEN
C           We've seen this X value once, negate it
            TEMP(L2)=-TEMP(L2)
            GOTO 100
          ENDIF
        ENDDO
        IDX=IDX+1
        TEMP(IDX)=CHECKX(L1)
  100   CONTINUE
      ENDDO
C
      TEMP(1)=CHECKY(1)
      JDX=1
      DO L1=2,NUM
        DO L2=1,JDX
          IF (TEMP(L2).EQ.CHECKY(L1)) THEN
C           We've seen this Y value once, negate it
            TEMP(L2)=-TEMP(L2)
            GOTO 200
          ENDIF
        ENDDO
        JDX=JDX+1
        TEMP(JDX)=CHECKY(L1)
  200   CONTINUE
      ENDDO
C     If the condition is true, we should have half the
C     number of COORDS with matching X and Y values
      IF ((IDX.EQ.NUM/2).AND.(JDX.EQ.NUM/2)) THEN
        CHECK2RED=.TRUE.
      ELSE
        CHECK2RED=.FALSE.
      ENDIF
      RETURN
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
C
      SUBROUTINE INSORT(LIST,MAXL,NUM)
C     Very, very slow insertion sort
C     LIST is returned with the first NUM elements
C     in descending order sorted on LIST(*,1)
      INTEGER*8 LIST(MAXL,5),TEMP(5)
      INTEGER MAXL,NUM,J,K,L1
      DO J=2,NUM
        IF ((MOD(J,10000).EQ.0).OR.(J.EQ.NUM)) THEN
C         WRITE(*,*)"Sort iteration ",J
        ENDIF
        DO L1=1,5
          TEMP(L1)=LIST(J,L1)
        ENDDO
        K=J-1
        DO WHILE ((LIST(K,1).LT.TEMP(1)).AND.(K.GE.1))
          DO L1=1,5
            LIST(K+1,L1)=LIST(K,L1)
          ENDDO
          K=K-1
        ENDDO
        DO L1=1,5
          LIST(K+1,L1)=TEMP(L1)
        ENDDO
      ENDDO
      RETURN
      END
C
      LOGICAL FUNCTION CHECKRECT(X1,Y1,X2,Y2,V,H,EL,COORDS,NC)
      INTEGER*8 X1,Y1,X2,Y2,XL,XR,YT,YB
      INTEGER*8 HY,VX,VY1,VY2,HX1,HX2,L1,L2
      INTEGER EL,V(EL,3),H(EL,3),NC
      COMPLEX COORDS(NC),RTEST(4)
      LOGICAL FOUND
C
   10 FORMAT(A,3I8)
C
C    *** Algorithm = check to see if any boundary lies entirely ***
C    *** within a rectangle. If there is at least one, the      ***
C    *** rectangle is not valid, if there are none it is valid. ***
C
C     Put rectangle coordinates into order L->R and Top -> Bottom
      IF (X1.LT.X2) THEN 
        XL=X1
        XR=X2
      ELSE
        XL=X2
        XR=X1
      ENDIF
      IF (Y1.LT.Y2) THEN
        YT=Y1
        YB=Y2
      ELSE
        YT=Y2
        YB=Y1
      ENDIF
C     WRITE(*,10)"Rectangle TL is ",XL,YT
      RTEST(1)=CMPLX(XL,YT)
C     WRITE(*,10)"Rectangle TR is ",XR,YT
      RTEST(2)=CMPLX(XR,YT)
C     WRITE(*,10)"Rectangle BL is ",XL,YB
      RTEST(3)=CMPLX(XL,YB)
C     WRITE(*,10)"Rectangle BR is ",XR,YB
      RTEST(4)=CMPLX(XR,YB)
C     WRITE(*,*)""
C
      L1=1
      CHECKRECT=.TRUE.
      DO WHILE ((L1.LE.EL).AND.(CHECKRECT))
        VX=V(L1,1)
        VY1=V(L1,2)
        VY2=V(L1,3)
        HY=H(L1,1)
        HX1=H(L1,2)
        HX2=H(L1,3)
C       Check to see if any of these boundaries encroach into
C       the rectangle beyond its edges. This means the current
C       rectangle is not the correct one.
C       Boundary X constant, Y varies
        DO L2=VY1,VY2
          IF (((VX.GT.XL).AND.(VX.LT.XR)) .AND.
     +        ((L2.GT.YT).AND.(L2.LT.YB))) THEN
            CHECKRECT=.FALSE.
C           WRITE(*,10)"Vertical boundary encroaches ",VX,L2
            GOTO 999
          ENDIF
        ENDDO
C       Boundary Y constant, X varies
        DO L2=HX1,HX2
          IF (((HY.GT.YT).AND.(HY.LT.YB)) .AND.
     +        ((L2.GT.XL).AND.(L2.LT.XR))) THEN
            CHECKRECT=.FALSE.
C           WRITE(*,10)"Horizontal boundary encroaches ",L2,HY
            GOTO 999
          ENDIF
        ENDDO
C       Try the next pair of boundaries
        L1=L1+1
      ENDDO
  999 CONTINUE
      RETURN
      END
      SUBROUTINE HEAPSORT(LIST,MAXL,NUM)
C     O(n log n) sort. Based on Sedgewick, R., Algorithms (1983), p.136.
C     LIST is returned with the first NUM elements sorted
C     in descending order of the first element in the 2d array
C
      INTEGER*8 LIST(MAXL,5),TEMP(5)
      INTEGER MAXL,NUM
C
      INTEGER K,M,N,ONE,L1
C
      N=NUM
      M=N
      ONE=1
      DO K=(M/2),1,-1
        CALL DOWNHEAP(LIST,MAXL,K,N)
      ENDDO
      DO WHILE (N.GT.1)
        DO L1=1,5
          TEMP(L1)=LIST(1,L1)
        ENDDO
        DO L1=1,5
          LIST(1,L1)=LIST(N,L1)
        ENDDO
        DO L1=1,5
          LIST(N,L1)=TEMP(L1)
        ENDDO
        N=N-1
        CALL DOWNHEAP(LIST,MAXL,ONE,N)
      ENDDO
      RETURN
      END
C
      SUBROUTINE DOWNHEAP(LIST,MAXL,K,N)
      INTEGER*8 LIST(MAXL,5),V(5)
      INTEGER K,N
C
      INTEGER I,J,R,L1
C
      DO L1=1,5
        V(L1)=LIST(K,L1)
      ENDDO
      R=K
      DO WHILE (R.LE.(N/2))
        J=R+R
        IF (J.LT.N) THEN
          IF (LIST(J,1).GT.LIST(J+1,1)) THEN
            J=J+1
          ENDIF
        ENDIF
        IF (V(1).LE.LIST(J,1)) GOTO 999
        DO L1=1,5
          LIST(R,L1)=LIST(J,L1)
        ENDDO
        R=J
      ENDDO
  999 CONTINUE
      DO L1=1,5
        LIST(R,L1)=V(L1)
      ENDDO
      RETURN
      END
