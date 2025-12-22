      PROGRAM DAY8P1
C
      INTEGER*8 MAXPTS,MAXPAIRS
C     Sample input is 20 coordinate points, puzzle 1,000
      PARAMETER (MAXPTS=1000)
C     Sample wants 10 closest pairs, puzzle 1,000
      PARAMETER (MAXPAIRS=1000)
C
      COMPLEX*8 DISTANCES((MAXPTS*(MAXPTS+1))/2)
      COMPLEX*8 REFS((MAXPTS*(MAXPTS+1))/2)
      COMPLEX*8 CLOSEREFS(MAXPAIRS)
      INTEGER*8 X(MAXPTS),Y(MAXPTS),Z(MAXPTS),PTS,PAIRS
      INTEGER*8 TOTAL,SX,SY,SZ
      INTEGER*8 CALCGROUP
      INTEGER*8 L1,L2
C
   10 FORMAT(A)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2025 day 8, part 1"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day8in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      PTS=1
   50 CONTINUE
C     Read each input line - can use list directed i/o
      READ(10,FMT=*,ERR=100,END=100)X(PTS),Y(PTS),Z(PTS)
      PTS=PTS+1
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C     PTS is 1 more than the number of coordinates read, decrement
      PTS=PTS-1
C     Work out the square of distances between all pairs of 3d points
      PAIRS=0
      DO L1=1,PTS-1
        DO L2=L1+1,PTS
          PAIRS=PAIRS+1
          SX=(X(L1)-X(L2))*(X(L1)-X(L2))
          SY=(Y(L1)-Y(L2))*(Y(L1)-Y(L2))
          SZ=(Z(L1)-Z(L2))*(Z(L1)-Z(L2))
C     Store the square of the distance with a unique number for
C     each distance (= unique pair of coordinates)
          DISTANCES(PAIRS)=DCMPLX(SX+SY+SZ,PAIRS)
C     Store a lookup for each coordinate pair so we can work out
C     which pairs of coordinates belong to each distance once sorted
C     Note - we don't care about the original X,Y,Z once the distances
C     between each coordinate is calculated, just need a reference to
C     each one instead. Makes life simpler!
          REFS(PAIRS)=DCMPLX(L1,L2)
        ENDDO
      ENDDO
C     Sort the DISTANCES low to high - do this by sorting on
C     the real part of the complex number held in DISTANCES
      CALL HEAPSORT(DISTANCES,PAIRS)
C     Get the references of the MAXPAIRS closest pairs
      DO L1=1,MAXPAIRS
        IDX=AIMAG(DISTANCES(L1))
        CLOSEREFS(L1)=REFS(IDX)
      ENDDO
C     Sort the pairs by their REAL component - makes grouping easier
      CALL HEAPSORT(CLOSEREFS,MAXPAIRS)
C     Group the MAXPAIRS nearest junction boxes into the largest
C     possible circuits, return the value of the sizes of the
C     three largest circuits multiplied together
      TOTAL=CALCGROUP(CLOSEREFS,MAXPAIRS)
      WRITE(*,10)" "
      WRITE(*,30)
     +   "Size of largest 3 circuits multiplied together is",TOTAL
      END
C
      FUNCTION CALCGROUP(CPAIRS,NUM)
      INTEGER*8 CALCGROUP
      COMPLEX*8 CPAIRS(*)
      INTEGER*8 NUM
C
   10 FORMAT(A,3I5)
   20 FORMAT(1000I4)
C
      INTEGER L1,L2,L3,L4,L5,L6,NOTE1,NOTE2
      INTEGER*8 T1,T2,T3,IDX,JDX
      INTEGER*8 CLENS(1000),CC(1000,2000)
C
C     Initialise the circuit lengths to 0
      DO L1=1,1000
        CLENS(L1)=0
      ENDDO
C     Step 1 - create unique circuit groups from the pairs
      L1=1
      DO WHILE (L1.LE.NUM)
C       Record the REAL part in the group as first element
C       and the IMAG part as second element. Only record the
C       REAL part once! Each circuit group will have unique elements
C       within the group but there may be overlap between the groups.
        IDX=INT8(CPAIRS(L1))
        JDX=1
        CC(IDX,JDX)=IDX
        DO WHILE(IDX.EQ.INT8(CPAIRS(L1)))
          JDX=JDX+1
          CC(IDX,JDX)=INT8(AIMAG(CPAIRS(L1)))
          L1=L1+1
        ENDDO
C       Record circuit group length
        CLENS(IDX)=JDX
C       WRITE(*,20)(CC(IDX,L2),L2=1,CLENS(IDX))
      ENDDO
C     WRITE(*,*)" "
C     Step 2 - count the number of non-empty circuit groups
      NOTE1=0
      DO L1=1,1000
        IF (CLENS(L1).NE.0) NOTE1=NOTE1+1 
      ENDDO
  100 CONTINUE
C     Step 3 - merge groups with overlapping circuits
      DO L1=1,1000
        IF (CLENS(L1).NE.0) THEN
C         WRITE(*,*)"Examining group ",L1
          DO L2=L1+1,1000
            IF (CLENS(L2).NE.0) THEN
              DO L3=1,CLENS(L1)
                DO L4=1,CLENS(L2)
                  IF (CC(L1,L3).EQ.CC(L2,L4)) THEN
C                   WRITE(*,*)L1,L2," jbox overlaps ",CC(L2,L4)
                    DO L5=CLENS(L1)+1,CLENS(L1)+CLENS(L2)
                      DO L6=1,CLENS(L1)
                        IF (CC(L1,L6).EQ.CC(L2,L5-CLENS(L1))) THEN
C                         WRITE(*,*)"Duplicate box ",CC(L1,L6)
                        ENDIF
                      ENDDO
                      CC(L1,L5)=CC(L2,L5-CLENS(L1))
                    ENDDO
                    CLENS(L1)=CLENS(L1)+CLENS(L2)
                    CLENS(L2)=0
C                   We have at least 1 duplicate in the extended circuit group
C                   WRITE(*,20)(CC(L1,L5),L5=1,CLENS(L1))
C                   Remove the duplicates
                    CALL DEDUP(CC(L1,1:L5),CLENS(L1))
C                   WRITE(*,20)(CC(L1,L5),L5=1,CLENS(L1))
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C     Step 4 - count the new not empty groups
      NOTE2=0
      DO L1=1,1000
        IF (CLENS(L1).NE.0) NOTE2=NOTE2+1 
      ENDDO
C     Repeat step 3 if we're still merging groups
      IF (NOTE1.NE.NOTE2) THEN
        WRITE(*,*)"Number of groups was ",NOTE1," now",NOTE2
        NOTE1=NOTE2
        GOTO 100
      ENDIF
C     WRITE(*,*)"Number of groups was ",NOTE1," now",NOTE2
C     Step 5 - find the 3 largest groups
      T1=0
      T2=0
      T3=0
      DO L1=1,1000
        IF (CLENS(L1).GT.T1) THEN
          T3=T2
          T2=T1
          T1=CLENS(L1)
        ELSE IF (CLENS(L1).GT.T2) THEN
          T3=T2
          T2=CLENS(L1)
        ELSE IF (CLENS(L1).GT.T3) THEN
          T3=CLENS(L1)
        ENDIF
      ENDDO
      WRITE(*,*)" "
      WRITE(*,10)"Largest circuits are of size ",T1,T2,T3
      CALCGROUP=T1*T2*T3
      RETURN
      END
C     
      SUBROUTINE INSORT(LIST,NUM)
C     O(n squared) sort
C     Very, very slow insertion sort for complex numbers
C     LIST is returned with the first NUM elements sorted
C     in ascending order of their REAL part, expressed as an integer*8
      COMPLEX*8 LIST(*),TEMP
      INTEGER*8 NUM,J,K
      DO J=2,NUM
        IF (MOD(J,50000).EQ.0) WRITE(*,*)"Sort iteration ",J
        IF (J.EQ.NUM) WRITE(*,*)"Final sort iteration ",J
        TEMP=LIST(J)
        K=J-1
        DO WHILE ((INT8(LIST(K)).GT.INT8(TEMP))
     +            .AND.(K.GE.1))
          LIST(K+1)=LIST(K)
          K=K-1
        ENDDO
        LIST(K+1)=TEMP
      ENDDO
      WRITE(*,*)" "
      RETURN
      END
C
      SUBROUTINE DEDUP(LIST,NUM)
      INTEGER*8 LIST(1,*),NUM
C
      INTEGER*8 L1,L2,TEMP(NUM),IDX
C
      IDX=1
      TEMP(IDX)=LIST(1,1)
      DO L1=2,NUM
        DO L2=1,IDX
          IF (LIST(1,L1).EQ.TEMP(L2)) GOTO 100
        ENDDO
        IDX=IDX+1
        TEMP(IDX)=LIST(1,L1)
  100   CONTINUE
      ENDDO
C     Set up the return values
      DO L1=1,IDX
        LIST(1,L1)=TEMP(L1)
      ENDDO
      NUM=IDX
      RETURN
      END
C
      SUBROUTINE BUBBLESORT1(LIST,NUM)
C     O(n squared) sort
C     LIST is returned with the first NUM elements sorted
C     in ascending order of their REAL part, expressed as an integer*8
C
      COMPLEX*8 LIST(*)
      INTEGER*8 NUM
C
      COMPLEX*8 TEMP
      INTEGER*8 ITER,L1
C
      ITER=1
      DO WHILE (ITER.LT.NUM)
        IF (MOD(ITER,50000).EQ.0)
     +    WRITE(*,*)ITER," list iterations completed"
        DO L1=NUM-1,ITER,-1
          IF (INT8(LIST(L1+1)).LT.INT8(LIST(L1))) THEN
            TEMP=LIST(L1)
            LIST(L1)=LIST(L1+1)
            LIST(L1+1)=TEMP
          ENDIF
        ENDDO
        ITER=ITER+1
      ENDDO
      WRITE(*,*)"All",NUM," list iterations completed"
      WRITE(*,*)" "
      RETURN
      END
      SUBROUTINE BUBBLESORT2(LIST,NUM)
C     O(n squared) sort
C     LIST is returned with the first NUM elements sorted
C     in ascending order of their REAL part, expressed as an integer*8
C
      COMPLEX*8 LIST(*)
      INTEGER*8 NUM
C
      COMPLEX*8 TEMP
      INTEGER*8 ITER,SKIPITER,L1
C
      ITER=1
      DO WHILE (ITER.LT.NUM)
        IF (MOD(ITER,50000).EQ.0)
     +    WRITE(*,*)ITER," list iterations completed"
        SKIPITER=NUM-1
        DO L1=NUM-1,ITER,-1
          IF (INT8(LIST(L1+1)).LT.INT8(LIST(L1))) THEN
            TEMP=LIST(L1)
            LIST(L1)=LIST(L1+1)
            LIST(L1+1)=TEMP
            SKIPITER=L1
          ENDIF
        ENDDO
        ITER=SKIPITER+1
      ENDDO
      WRITE(*,*)"All",NUM," list iterations completed"
      WRITE(*,*)" "
      RETURN
      END
C
      SUBROUTINE HEAPSORT(LIST,NUM)
C     O(n log n) sort. Based on Sedgewick, R., Algorithms (1983), p.136.
C     LIST is returned with the first NUM elements sorted
C     in ascending order of their REAL part, expressed as an integer*8
C
      COMPLEX*8 LIST(*),T
      INTEGER*8 NUM
C
      INTEGER*8 K,M,N,ONE
C
      N=NUM
      M=N
      ONE=1
      DO K=(M/2),1,-1
        CALL DOWNHEAP(LIST,K,N)
      ENDDO
      DO WHILE (N.GT.1)
        T=LIST(1)
        LIST(1)=LIST(N)
        LIST(N)=T
        N=N-1
        CALL DOWNHEAP(LIST,ONE,N)
      ENDDO
      RETURN
      END
C
      SUBROUTINE DOWNHEAP(LIST,K,N)
      COMPLEX*8 LIST(*),V
      INTEGER*8 K,N
C
      INTEGER*8 I,J,R
C
      V=LIST(K)
      R=K
      DO WHILE (R.LE.(N/2))
        J=R+R
        IF (J.LT.N) THEN
          IF (INT8(LIST(J)).LT.INT8(LIST(J+1))) THEN
            J=J+1
          ENDIF
        ENDIF
        IF (INT8(V).GE.INT8(LIST(J))) GOTO 999
        LIST(R)=LIST(J)
        R=J
      ENDDO
  999 CONTINUE
      LIST(R)=V
      RETURN
      END
