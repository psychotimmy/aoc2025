      PROGRAM DAY4P1
C     Puzzle grid is 137x137 - need grid of size 139,139
      INTEGER PSIZE
      PARAMETER (PSIZE=139)
C
      INTEGER GRID(PSIZE,PSIZE),TOTAL,X,Y,WALKGRID
      CHARACTER*(PSIZE-2) STR
C
   10 FORMAT(A)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2024 day 4, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day4in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Pad the top & botton rows of the grid with 0's to
C     make the calculation of adjacent rolls easier
      DO X=1,PSIZE
        GRID(X,1)=0
        GRID(X,PSIZE)=0
      ENDDO
C     Grid is 0 padded, so start at column 2
      Y=2
   50 CONTINUE
C     Read each input line and place onto the grid
C     . = 0, @ = 1
      READ(10,FMT=10,ERR=100,END=100)STR
      CALL GRIDINIT(GRID,STR,Y,PSIZE)
      Y=Y+1
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C     Walk the grid, find the rolls that can be removed.
      TOTAL=WALKGRID(GRID,PSIZE)
      WRITE(*,30)"The number of removable rolls is ",TOTAL
      END
C
      SUBROUTINE GRIDINIT(GRID,STR,Y,SIZE)
      INTEGER SIZE,GRID(SIZE,SIZE),Y
      CHARACTER*(*)STR
C
      INTEGER L1
C
C     Pad the edge of the grid with 0
C
      GRID(1,Y)=0
      GRID(SIZE,Y)=0
      DO L1=1,SIZE-2
C       Mark no roll with 0
        IF (STR(L1:L1).EQ.'.') GRID(L1+1,Y)=0
C       Mark roll with 1
        IF (STR(L1:L1).EQ.'@') GRID(L1+1,Y)=1
      ENDDO
      RETURN
      END
C
      FUNCTION WALKGRID(GRID,SIZE)
      INTEGER WALKGRID,SIZE,GRID(SIZE,SIZE)
C
      INTEGER XTEST,YTEST,VALUE
      WALKGRID=0
      DO YTEST=2,SIZE-1
        DO XTEST=2,SIZE-1
          IF (GRID(XTEST,YTEST).EQ.1) THEN
            VALUE=GRID(XTEST-1,YTEST-1)+
     +            GRID(XTEST,YTEST-1)+
     +            GRID(XTEST+1,YTEST-1)+
     +            GRID(XTEST+1,YTEST)+
     +            GRID(XTEST+1,YTEST+1)+
     +            GRID(XTEST,YTEST+1)+
     +            GRID(XTEST-1,YTEST+1)+
     +            GRID(XTEST-1,YTEST)
            IF (VALUE.LT.4) WALKGRID=WALKGRID+1
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
